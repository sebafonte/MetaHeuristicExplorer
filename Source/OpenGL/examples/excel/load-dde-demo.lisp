;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/excel:load-dde-demo.lisp,v 1.3.6.1 2007/10/23 22:17:08 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(load (current-pathname "../../host"))
(load (current-pathname "../load"))

(require "dde")

(load (current-pathname "demo" nil))

;; Use (demo) to start the viewer.

;; Open the spreadsheet and click on the pictures to transfer the data
;; from Excel to LispWorks.
