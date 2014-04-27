(capi:define-interface interface-1 ()
  ()
  (:panes
   (multi-line-text-input-pane-1
    capi:multi-line-text-input-pane
    :allows-newline-p t
    :enabled t
    :multi-line-p t)
   (multi-line-text-input-pane-2
    capi:multi-line-text-input-pane)
   (multi-line-text-input-pane-3
    capi:multi-line-text-input-pane)
   (editor-pane-1
    capi:editor-pane))
  (:layouts
   (column-layout-1
    capi:column-layout
    '(multi-line-text-input-pane-1 multi-line-text-input-pane-2 multi-line-text-input-pane-3 editor-pane-1)
    :enabled t))
  (:default-initargs
   :best-height 499
   :best-width 275
   :layout 'column-layout-1
   :title "Interface-1"))
