;;;;;
;;;;; File: s-dot.asd
;;;;;
;;;;; A Common Lisp language binding for the graphviz 'dot' tool.
;;;;; See http://www.martin-loetzsch.de/S-DOT for details
;;;;;
;;;;; Copyright (c) 2006-2010 Martin Loetzsch.  All rights reserved.
;;;;;
;;;;; Redistribution and use in source and binary forms, with or without
;;;;; modification, are permitted provided that the following conditions
;;;;; are met:
;;;;;
;;;;; 1. Redistributions of source code must retain the above copyright
;;;;;    notice, this list of conditions and the following disclaimer.
;;;;;
;;;;; 2. Redistributions in binary form must reproduce the above
;;;;;    copyright  notice, this list of conditions and the following
;;;;;    disclaimer in the documentation and/or other materials provided
;;;;;    with the distribution.
;;;;;
;;;;; 3. The end-user documentation included with the redistribution, if
;;;;;    any, must include the following acknowledgment:
;;;;;    "This product includes S-DOT developed by Martin Loetzsch
;;;;;    (http://www.martin-loetzsch.de/S-DOT)."
;;;;;    Alternately, this acknowledgment may appear in the software
;;;;;    itself, if and wherever such third-party acknowledgments
;;;;;    normally appear.
;;;;;
;;;;; THIS SOFTWARE IS PROVIDED BY MARTIN LOETZSCH ``AS IS'' AND ANY
;;;;; EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;;;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL MARTIN LOETZSCH BE LIABLE
;;;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :asdf)

(defsystem :s-dot2
  :description "Render Graphviz graphs from within Lisp"
  :long-description "See http://martin-loetzsch.de/S-DOT/"
  :author "Martin Loetzsch, cage"
  :licence "BSD-style"
  :version "1.3.0"
  :depends-on ("uiop")
  :components ((:file "s-dot2")))
