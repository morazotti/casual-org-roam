;;; casual-org-roam.el --- Casual Menu para Org-Roam & Org-QL -*- lexical-binding: t; -*-

;; Author: Nicolas <nicolas.morazotti@gmail.com>
;; Keywords: org-roam, org-ql, transient, casual
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (org-roam "2.2.0") (org-ql "0.8"))

;;; Commentary:
;; Menu Casual (Transient) combinando Org-Roam com consultas avançadas Org-QL,
;; incluindo um construtor interativo de predicados via completing-read.

;;; Code:

(require 'transient)
(require 'org-roam)
(require 'org-ql)
(require 'org-ql-search)
(require 'org-roam-ql)
(require 'consult-org-roam nil t)

;; ---------------------------------------------------------------------
;; BUILDER ORG-QL COM COMPLETING-READ
;; ---------------------------------------------------------------------

(defun casual-roam-ql--read-tag ()
  "Lê tag existente do Org-Roam via `completing-read'."
  (let ((tags (if (fboundp 'org-roam-tag-completions)
                  (org-roam-tag-completions)
                nil)))
    (completing-read "Tag: " tags nil nil)))

(defun casual-roam-ql--read-todo-state ()
  "Lê estado TODO relevante via `completing-read'."
  (let ((states (append '("QUALQUER TODO")
                        (or (bound-and-true-p org-todo-keywords-1)
                            '("TODO" "WAIT" "HOLD" "DONE" "CANCELLED")))))
    (completing-read "Estado TODO: " states nil t)))

;;;###autoload
(defun casual-roam-ql-builder ()
  "Construtor interativo de consultas Org-QL no Org-Roam com `completing-read'.
Permite compor múltiplos critérios (AND/OR), escolher ordenação e executar."
  (interactive)
  (let ((clauses nil)
        (looping t))
    (while looping
      (let* ((prompt-info (if clauses
                              (format "Critérios [%s] -> próximo: "
                                      (mapconcat (lambda (c) (format "%S" c)) clauses ", "))
                            "Primeiro critério: "))
             (options '(("[Finalizar e Buscar]"  . done)
                        ("Tag do Roam"           . tag)
                        ("Estado TODO"           . todo)
                        ("Texto no Cabeçalho"    . heading)
                        ("Texto Geral (Regex)"   . regexp)
                        ("Prioridade"            . priority)
                        ("Modificado recente"    . recent)
                        ("Com Prazo (Deadline)"  . deadline)
                        ("Com Agendamento"       . scheduled)
                        ("Expressão Lisp crua"   . raw)))
             (choice-label (completing-read prompt-info (mapcar #'car options) nil t))
             (action (cdr (assoc choice-label options))))
        (pcase action
          ((or 'nil 'done)
           (setq looping nil))
          ('tag
           (let ((tag (casual-roam-ql--read-tag)))
             (when (> (length tag) 0)
               (push `(tags ,tag) clauses))))
          ('todo
           (let ((st (casual-roam-ql--read-todo-state)))
             (push (if (string-equal st "QUALQUER TODO")
                       '(todo)
                     `(todo ,st))
                   clauses)))
          ('heading
           (let ((h (read-string "Texto no cabeçalho: ")))
             (when (> (length h) 0)
               (push `(heading ,h) clauses))))
          ('regexp
           (let ((rg (read-string "Expressão regular: ")))
             (when (> (length rg) 0)
               (push `(regexp ,rg) clauses))))
          ('priority
           (let ((p (completing-read "Prioridade: " '("A" "B" "C") nil t)))
             (push `(priority ,p) clauses)))
          ('recent
           (let ((d (read-number "Modificado nos últimos N dias: " 7)))
             (push `(ts :from ,(- (abs d))) clauses)))
          ('deadline
           (let ((dl (completing-read "Deadline: " '("Atrasados (past)" "Hoje (today)" "Futuros (future)") nil t)))
             (pcase dl
               ("Atrasados (past)" (push '(deadline :past t) clauses))
               ("Hoje (today)"     (push '(deadline :to today) clauses))
               ("Futuros (future)" (push '(deadline :from today) clauses)))))
          ('scheduled
           (let ((sc (completing-read "Scheduled: " '("Atrasados (past)" "Hoje (today)" "Futuros (future)") nil t)))
             (pcase sc
               ("Atrasados (past)" (push '(scheduled :past t) clauses))
               ("Hoje (today)"     (push '(scheduled :to today) clauses))
               ("Futuros (future)" (push '(scheduled :from today) clauses)))))
          ('raw
           (let ((r (read-string "S-expression crua: ")))
             (when (> (length r) 0)
               (condition-case nil
                   (push (read r) clauses)
                 (error (message "Expressão inválida descartada.")))))))))

    (if (null clauses)
        (message "Nenhum critério selecionado.")
      (let* ((clauses-clean (nreverse clauses))
             (logic-op (if (> (length clauses-clean) 1)
                           (intern (completing-read "Operador lógico: " '("and" "or") nil t nil nil "and"))
                         'and))
             (query (if (= (length clauses-clean) 1)
                        (car clauses-clean)
                      (cons logic-op clauses-clean)))
             (sort-options '(("priority" . '(priority))
                             ("date"     . '(date))
                             ("todo"     . '(todo priority))
                             ("nenhum"   . nil)))
             (sort-choice (cdr (assoc (completing-read "Ordenar por: "
                                                       (mapcar #'car sort-options)
                                                       nil t nil nil "nenhum")
                                      sort-options)))
             (scope-choice (completing-read "Escopo da busca: "
                                            '("Org-Roam" "Agenda Files")
                                            nil t nil nil "Org-Roam"))
             (title (format "%s: %S" scope-choice query)))
        (if (string-equal scope-choice "Org-Roam")
            ;; Usa org-roam-ql para busca eficiente no banco de dados do Roam
            (if sort-choice
                (org-roam-ql-search query :title title :sort (eval sort-choice t))
              (org-roam-ql-search query :title title))
          ;; Fallback para org-ql-search clássico para Agenda Files
          (let ((target-files (org-agenda-files)))
            (if sort-choice
                (org-ql-search target-files query :title title :sort (eval sort-choice t))
              (org-ql-search target-files query :title title))))))))

;; ---------------------------------------------------------------------
;; CONSULTAS PREDEFINIDAS ORG-QL
;; ---------------------------------------------------------------------

(defun casual-roam-ql-todos ()
  "Listar tarefas (TODOs) no acervo do Org-Roam."
  (interactive)
  (org-roam-ql-search '(todo)
                      :title "Org-Roam: Tarefas Perdidas (TODOs)"
                      :sort '(priority todo)))

(defun casual-roam-ql-recent ()
  "Listar nós do Org-Roam com alterações nos últimos 7 dias."
  (interactive)
  (org-roam-ql-search '(ts :from -7)
                      :title "Org-Roam: Modificados recentemente (7 dias)"
                      :sort '(date)))

(defun casual-roam-ql-projects ()
  "Listar nós com tag 'project' no Org-Roam."
  (interactive)
  (org-roam-ql-search '(tags "project")
                      :title "Org-Roam: Projetos Ativos"))

(defun casual-roam-ql-search (query)
  "Busca livre com Org-QL restrita aos arquivos do Org-Roam."
  (interactive (list (read-string "Busca Org-QL no Roam: ")))
  (let ((query-form (condition-case nil
                        (read query)
                      (error query))))
    (org-roam-ql-search query-form
                        :title (format "Org-Roam QL: %s" query))))

(defun casual-roam-ql-find ()
  "Filtro dinâmico com Org-QL sobre as notas do Roam."
  (interactive)
  (org-ql-find (org-roam-list-files)
    :prompt "Filtrar notas (Org-QL): "))

(defun casual-roam-ql-agenda-today ()
  "Foco de hoje na Agenda com Org-QL."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(ts :to today)
    :title "Agenda: Foco de Hoje"
    :sort '(priority date)))

(defun casual-roam-ql-agenda-todos ()
  "Todos os TODOs da Agenda com Org-QL."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(todo)
    :title "Agenda: Todos os TODOs"
    :sort '(todo priority)))

(defun casual-roam-ql-agenda-overdue ()
  "Tarefas atrasadas e urgentes da Agenda."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo) (ts :past t))
    :title "Agenda: Atrasados e Urgentes"
    :sort '(priority date)))

(defun casual-roam-open-graph ()
  "Abrir grafo visual (Org-Roam-UI ou Org-Roam Graph)."
  (interactive)
  (cond
   ((fboundp 'org-roam-ui-open) (org-roam-ui-open))
   ((fboundp 'org-roam-graph)   (org-roam-graph))
   (t (user-error "Nenhum visualizador de grafo disponível"))))

;; ---------------------------------------------------------------------
;; SUBMENU: Consultas Org-Roam
;; ---------------------------------------------------------------------

(transient-define-prefix casual-org-ql-roam ()
  "Submenu para caçar coisas no seu segundo cérebro."
  [["Consultas no Org-Roam"
    ("B" "Construtor de Buscas…"         casual-roam-ql-builder)
    ("t" "Tarefas perdidas (TODOs)"      casual-roam-ql-todos)
    ("r" "Modificados recentemente"      casual-roam-ql-recent)
    ("p" "Projetos Ativos"               casual-roam-ql-projects)
    ("s" "Busca Org-QL livre…"           casual-roam-ql-search)]
   ["Navegação"
    ("b" "Voltar ao menu QL"             casual-org-ql)
    ("q" "Sair"                          transient-quit-one)]])

;; ---------------------------------------------------------------------
;; MENU INTERMEDIÁRIO: Casual Org-QL
;; ---------------------------------------------------------------------

(transient-define-prefix casual-org-ql ()
  "Painel de consultas e filtros Org-QL."
  [["Construtor & Filtros"
    ("B" "Query Builder (Completing-Read)" casual-roam-ql-builder)
    ("c" "Filtro dinâmico (find)…"         casual-roam-ql-find)
    ("l" "Busca livre (ql-search)…"        org-ql-search)
    ("v" "Abrir Views Salvas"              org-ql-view)]
   ["Segundo Cérebro (Roam)"
    ("r" "TODOs no Roam"                   casual-roam-ql-todos)
    ("m" "Modificados (7 dias)"            casual-roam-ql-recent)
    ("p" "Projetos Ativos"                 casual-roam-ql-projects)
    ("s" "Busca QL no Roam…"               casual-roam-ql-search)
    ("g" "Grep textual no Roam…"           consult-org-roam-search)]
   ["Agenda Automágica"
    ("a" "Foco de Hoje"                    casual-roam-ql-agenda-today)
    ("t" "Todos os TODOs (Caos)"           casual-roam-ql-agenda-todos)
    ("d" "Atrasados e Urgentes (Pânico)"   casual-roam-ql-agenda-overdue)]]
  [["Navegação"
    ("b" "Voltar ao Roam Master"           casual-org-roam-master)
    ("C-g" "Desistir e ir trabalhar"         transient-quit-one)]])

;; ---------------------------------------------------------------------
;; MENU PRINCIPAL: Casual Org-Roam Master
;; ---------------------------------------------------------------------

(transient-define-prefix casual-org-roam-master ()
  "O Centro de Comando Casual para Org-Roam."
  [["Criação & Navegação"
    ("f" "Buscar Nó (Find)"        org-roam-node-find)
    ("i" "Inserir Link (Insert)"   org-roam-node-insert)
    ("c" "Capturar Nota (Capture)" org-roam-capture)
    ("b" "Buffer Roam (Toggle)"    org-roam-buffer-toggle)
    ("r" "Nó Aleatório (Random)"   org-roam-node-random)]
   ["Cirurgia de Metadados"
    ("t" "Adicionar Tag"           org-roam-tag-add)
    ("x" "Remover Tag"             org-roam-tag-remove)
    ("a" "Adicionar Alias"         org-roam-alias-add)
    ("X" "Remover Alias"           org-roam-alias-remove)]
   ["Arsenal de Consultas"
    ("B" "Query Builder›"          casual-roam-ql-builder)
    ("s" "Grep no Acervo (Ripgrep)" consult-org-roam-search)
    ("q" "Painel Org-QL›"          casual-org-ql-tmenu)
    ("g" "Grafo Visual"            casual-roam-open-graph)
    ("u" "Sincronizar DB"          org-roam-db-sync)]]
  [["Controle"
    ("C-g" "Desistir e ir simular fluidos" transient-quit-one)]])

;; Aliases no padrão Casual Suite
(defalias 'casual-org-roam-tmenu #'casual-org-roam-master)
(defalias 'casual-org-ql-tmenu   #'casual-org-ql)

(provide 'casual-org-roam)
;;; casual-org-roam.el ends here
