;;; teste.el --- Casual Menu para Org-Roam & Org-QL -*- lexical-binding: t; -*-

(require 'transient)
(require 'org-roam)
(require 'org-ql)
(require 'org-ql-search)
(require 'consult-org-roam nil t)

;; ---------------------------------------------------------
;; COMANDOS AUXILIARES: Consultas Org-QL
;; ---------------------------------------------------------

(defun casual-roam-ql-todos ()
  "Listar tarefas (TODOs) no acervo do Org-Roam."
  (interactive)
  (org-ql-search (org-roam-list-files)
    '(todo)
    :title "Org-Roam: Tarefas Perdidas (TODOs)"
    :sort '(priority todo)))

(defun casual-roam-ql-recent ()
  "Listar nós do Org-Roam com alterações nos últimos 7 dias."
  (interactive)
  (org-ql-search (org-roam-list-files)
    '(ts :from -7)
    :title "Org-Roam: Modificados recentemente (7 dias)"
    :sort '(date)))

(defun casual-roam-ql-projects ()
  "Listar projetos ativos marcados no Org-Roam."
  (interactive)
  (org-ql-search (org-roam-list-files)
    '(tags "project")
    :title "Org-Roam: Projetos Ativos"))

(defun casual-roam-ql-search (query)
  "Busca livre com Org-QL restrita aos arquivos do Org-Roam."
  (interactive (list (read-string "Busca Org-QL no Roam: ")))
  (let ((query-form (condition-case nil
                        (read query)
                      (error query))))
    (org-ql-search (org-roam-list-files)
      query-form
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

;; ---------------------------------------------------------
;; SUBMENU: Consultas Org-Roam
;; ---------------------------------------------------------

(transient-define-prefix casual-org-ql-roam ()
  "Submenu para caçar coisas no seu segundo cérebro."
  [["Consultas no Org-Roam"
    ("t" "Tarefas perdidas (TODOs)"        casual-roam-ql-todos)
    ("r" "Modificados nos últimos 7 dias" casual-roam-ql-recent)
    ("p" "Projetos Ativos"                 casual-roam-ql-projects)
    ("s" "Busca Org-QL livre…"             casual-roam-ql-search)]
   ["Navegação"
    ("b" "Voltar ao menu QL"               casual-org-ql)
    ("q" "Sair"                            transient-quit-one)]])

;; ---------------------------------------------------------
;; MENU INTERMEDIÁRIO: Casual Org-QL
;; ---------------------------------------------------------

(transient-define-prefix casual-org-ql ()
  "Painel de consultas e filtros Org-QL."
  [["Agenda Automágica"
    ("a" "Foco de Hoje"                  casual-roam-ql-agenda-today)
    ("t" "Todos os TODOs (Caos)"         casual-roam-ql-agenda-todos)
    ("d" "Atrasados e Urgentes (Pânico)" casual-roam-ql-agenda-overdue)
    ("v" "Abrir Views Salvas"            org-ql-view)]
   ["Segundo Cérebro (Roam)"
    ("r" "TODOs no Roam"                 casual-roam-ql-todos)
    ("m" "Modificados (7 dias)"          casual-roam-ql-recent)
    ("p" "Projetos Ativos"               casual-roam-ql-projects)
    ("s" "Busca QL no Roam…"             casual-roam-ql-search)]
   ["Operações Interativas"
    ("c" "Filtro dinâmico (find)…"       casual-roam-ql-find)
    ("l" "Busca Org-QL livre…"           org-ql-search)
    ("g" "Grep textual no Roam…"         consult-org-roam-search)]]
  [["Navegação"
    ("b" "Voltar ao Roam Master"         casual-org-roam-master)
    ("q" "Desistir e ir trabalhar"       transient-quit-one)]])

;; ---------------------------------------------------------
;; MENU PRINCIPAL: Casual Org-Roam Master
;; ---------------------------------------------------------

(transient-define-prefix casual-org-roam-master ()
  "O Centro de Comando Absoluto do Doutor."
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
    ("s" "Grep no Acervo (Ripgrep)" consult-org-roam-search)
    ("q" "Painel Org-QL›"           casual-org-ql)
    ("g" "Grafo Visual"             casual-roam-open-graph)
    ("u" "Sincronizar DB"           org-roam-db-sync)]]
  [["Controle"
    ("q" "Desistir e ir simular fluidos" transient-quit-one)]])

;; Aliases no estilo Casual Suite
(defalias 'casual-org-roam-tmenu #'casual-org-roam-master)
(defalias 'casual-org-ql-tmenu   #'casual-org-ql)

;; Mapeamentos globais
(global-set-key (kbd "C-c r") #'casual-org-roam-master)
(global-set-key (kbd "C-c q") #'casual-org-ql)

(provide 'casual-org-roam)
;;; teste.el ends here
