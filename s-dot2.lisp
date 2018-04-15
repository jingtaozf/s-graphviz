;;;;;
;;;;; File s-dot.lisp
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

(defpackage :s-dot
  (:use :common-lisp
        :uiop)
  (:documentation "See http://martin-loetzsch.de/S-DOT")
  (:export :check-syntax
           :s-dot->dot
           :*dot-exe*
           :render-s-dot
           :render-s-dot-sbcl))

(in-package :s-dot)

(defparameter *dot-exe* "dot"
  "the path (or name) name of the dot executable")

(defparameter *dot-output-format-switch* "-T"
  "the command line option for dot needed to specify the output format")

;; the pure syntax of s-dot.
;; http://martin-loetzsch.de/S-DOT contains the complete documentation
(defparameter s-dot-syntax
  `((:graph (:cluster :sub-graph :node :record :edge) ;; children
            nil ;; required attributes
            (:bgcolor :fontcolor :fontname ;; optional attributes, including the required ones
                      :fontsize :label :margin :nodesep :rankdir :ranksep :ratio :size))
    (:cluster (:cluster :sub-graph :node :record :edge)
              (:id)
              (:bgcolor :color :fillcolor :fontcolor :fontname :fontsize :id :label :labeljust
                        :labelloc :style))
    (:sub-graph (:node :record)
                (:rank)
                (:rank))
    (:node nil
           (:id)
           (:color :fillcolor :fixedsize :fontcolor :fontname :fontsize :height :id
                   :label :shape :style :width))
    (:record (:record :node)
             nil
             (:color :fillcolor :fixedsize :fontcolor :fontname :fontsize :height
                     :label :shape :style :width))
    (:edge nil
           (:from :to)
           (:arrowhead :arrowsize :arrowtail :color :constraint :decorate :dir
                       :fontcolor :fontname :fontsize :from :headlabel :headport
                       :label :labeldistance :labelfloat :labelfontcolor
                       :labelfontname :labelfontsize :lhead :ltail :minlen :style
                       :samehead :sametail :taillabel :tailport :to))))

(defun check-syntax (graph &key (level 0))
  "Checks whether the s-expression 'graph' is syntactically correct.
Note that it does not check whether the attributes contain useful values."
  (unless (and graph (listp graph)) (error "graph should be a list"))
  (let* ((element (first graph))
         (element-spec (find element s-dot-syntax :key #'first)))
    (when (and (= level 0) (not (equal element :graph)))
      (error "an s-dot expression should start with ~s. Instead, ~s was passed"
             :graph element))
    (unless (listp (second graph)) (error "~s should be a list of parameters, context: ~a"
                                          (second graph) graph))
    (loop for attribute in (second graph)
       do (when (or (not (listp attribute)) (not (= (length attribute) 2))
                    (not (stringp (second attribute))))
            (error "~s attribute ~s should be of the form (attribute \"value\"), context: "
                   element attribute))
         (unless (find (first attribute) (fourth element-spec))
           (error "attribute ~s is not allowed in ~s elements, context: ~s"
                  (first attribute) element graph)))
    (loop for required-attribute in (third element-spec)
       do (unless (find required-attribute (second graph) :key #'first)
            (error "required attribute ~s not found in element ~s, context: ~a"
                   required-attribute element graph)))
    (loop for child in (cddr graph)
       do (unless (listp child)
            (error "child ~s of element ~s should be a alist, context: ~a" child element graph))
         (unless (symbolp (first child))
           (error "~s should be the name of an element, context: ~a" (first child) child))
         (unless (find (first child)  s-dot-syntax :key #'first)
           (error "element ~s not known, context: ~a" (first child) child))
         (unless (find (first child) (second element-spec))
           (error "element ~s not allowed inside ~s, context: ~a"
                  (first child) element child))
         (check-syntax child :level (+ 1 level)))))

(defun escape-record-label (raw &key (escape-bag '(#\| #\{ #\} #\< #\> #\Space)))
  (if (string= raw "")
      ""
      (let ((examinated (elt raw 0)))
        (concatenate 'string
                     (if (find examinated escape-bag :test #'char=)
                         (format nil "\\~a" examinated)
                         (string examinated))
                     (escape-record-label (subseq raw 1))))))

(defun s-dot->dot (stream graph &key (check-syntax t))
  "Generates dot syntax from a s-dot expression and writes the result to 'stream'.
This code looks indeed ugly. If you really want to understand what's going on, then it might
be easier to look in the the XSLT stylesheet at
http://www.martin-loetzsch.de/DOTML/dotml-1.3/dotml2dot.xsl . This code does exactly the same."
  (let ((sub-graph-counter 0)
        (records nil))
    (labels ((attribute-value (attribute element)
               (second (find attribute (second element) :key #'first)))
             (generate-record-label (element)
               (if (equal (first element) :record)
                   (format nil "{~{~a~^ | ~}}" (mapcar #'generate-record-label (cddr element)))
                   (format nil "<~a> ~a" (attribute-value :id element)
                           (if (attribute-value :label element)
                               (escape-record-label (attribute-value :label element))
                               (attribute-value :id element)))))
             (write-attributes (element separator exclude-attributes)
               (let ((element-spec (find (first element) s-dot-syntax :key #'first)))
                 (loop for attribute in (fourth element-spec)
                    for attribute-value = (attribute-value attribute element)
                    do (unless (find attribute exclude-attributes)
                         (format stream "~a=\"~a\"~a"
                                 (string-downcase (symbol-name attribute))
                                 (if attribute-value attribute-value "")
                                 separator)))))
             (write-element
                 (element)
               (macrolet ((write-children ()
                            `(loop for e in (cddr element) do (write-element e))))
                 (cond ((equal (first element) :sub-graph)
                        (format stream "subgraph sub_graph_~a{rank=\"~a\";"
                                (incf sub-graph-counter) (attribute-value :rank element))
                        (write-children)
                        (format stream "}"))
                       ((equal (first element) :cluster)
                        (format stream "subgraph cluster_~a{" (attribute-value :id element))
                        (write-attributes element ";" '(:id))
                        (write-children)
                        (format stream "}"))
                       ((equal (first element) :node)
                        (format stream "node[label=\"~a\", "
                                (if (attribute-value :label element)
                                    (attribute-value :label element)
                                    (attribute-value :id element)))
                        (write-attributes element "," '(:label :id))
                        (format stream "] {~s};" (attribute-value :id element)))
                       ((equal (first element) :edge)
                        (format stream "edge[")
                        (write-attributes element "," '(:from :to :lhead :ltail))
                        (format stream "lhead=\"~a\", ltail=\"~a\"]"
                                (if (attribute-value :lhead element)
                                    (concatenate 'string "cluster_"
                                                 (attribute-value :lhead element)) "")
                                (if (attribute-value :ltail element)
                                    (concatenate 'string "cluster_"
                                                 (attribute-value :ltail element)) ""))
                        (let ((struct-from (loop for r in records
                                              when (find (attribute-value :from element)
                                                         (third r) :test #'equal)
                                              return (second r)))
                              (struct-to (loop for r in records
                                            when (find (attribute-value :to element)
                                                       (third r) :test #'equal)
                                            return (second r))))
                          (when struct-from (format stream "struct~a:" struct-from))
                          (format stream "~s -> " (attribute-value :from element))
                          (when struct-to (format stream "struct~a:" struct-to))
                          (format stream "~s;" (attribute-value :to element))))
                       ((equal (first element) :record)
                        (format stream "node[shape=\"record\",label=\"~a\""
                                (format nil "~{~a~^ | ~}"
                                        (mapcar #'generate-record-label (cddr element))))
                        (write-attributes element "," '(:id :label :shape))
                        (format stream "]{struct~a};"
                                (second (find element records :key #'first :test #'equal)))))))
             (store-record-nodes (graph)
               (if (equal (first graph) :node)
                   (list (second (find :id (second graph) :key #'first)))
                   (loop for element in (cddr graph)
                      append (store-record-nodes element))))
             (store-records (graph)
               (if (equal (first graph) :record)
                   (push (list  graph (length records)
                                (store-record-nodes graph)) records)
                   (loop for element in (cddr graph)
                      do (store-records element)))))
      (store-records graph)
      (when check-syntax (check-syntax graph))
      (format stream "digraph g {compound=\"true\";")
      (write-attributes graph ";" nil)
      (loop for element in (cddr graph) do (write-element element))
      (format stream "}~c" #\linefeed))))

(defun render-s-dot (file-name format graph &key (check-syntax t))
  "Renders a s-dot graph into a graphic file. 'file-name' should be a pathname.
If the file-name is /foo/bar.png, the dot file /foo/bar.dot is created
and    then    rendered.     Format    should   be    one    out    of
http://www.graphviz.org/doc/info/output.html,  for  example  svg,  ps,
gif, png, or jpg.  The uiop:run-program is used to launch dot. If that
does not work for you, write a similar function that uses (s-dot->dot)
for dot generation and then runs dot on it.

SBCL users may take advantage of sb-ext:run-program, opening a pipe to
'dot' instead of writing a file and reading that in again."

  #+sbcl (render-s-dot-sbcl file-name format graph :check-syntax check-syntax)
  #-sbcl (let ((dot-file-name (make-pathname :directory (pathname-directory file-name)
                                             :name (pathname-name file-name) :type "dot")))

           (with-open-file (stream dot-file-name :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
             (s-dot->dot stream graph :check-syntax check-syntax))
           (uiop:run-program (format nil "~a -o ~a ~a~a ~a"
                                     *dot-exe* file-name *dot-output-format-switch*
                                     format dot-file-name)
                             :ignore-error-status t)))

#+sbcl (defun render-s-dot-sbcl (file-name format graph &key (check-syntax t))
         (with-open-file (stream file-name :direction :output :if-exists :supersede
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
           (let ((process (sb-ext:run-program *dot-exe*
                                              (list (format nil "~a~a"
                                                            *dot-output-format-switch* format))
                                              :output :stream :input :stream
                                              :wait nil :search t)))
             (when process
               (let ((process-input-stream (sb-ext:process-input process))
                     (buffer (make-array 256 :element-type '(unsigned-byte 8))))
                 (s-dot->dot process-input-stream graph :check-syntax check-syntax)
                 (finish-output process-input-stream)
                 (close process-input-stream)
                 (do ((read (read-sequence buffer (sb-ext:process-output process))
                            (read-sequence buffer (sb-ext:process-output process))))
                     ((/= read (length buffer))
                      (write-sequence buffer stream :start 0 :end read))
                   (finish-output (sb-ext:process-output process))
                   (write-sequence buffer stream :start 0 :end read)))
               (sb-ext:process-exit-code (sb-ext:process-close process))))))

(defun test-s-dot ()
  "Generates a few charts in /tmp/. If you don't have /tmp/,
change the directory below to something else"
  (let ((directory '(:absolute "tmp"))
        (graph1 '(:graph ()
                  (:node ((:id "a") (:label "a")))
                  (:cluster ((:id "c1") (:label "c1") (:style "filled") (:fillcolor "#EEEEFF")
                             (:fontcolor "#900000") (:fontname "Arial bold") (:fontsize "15"))
                   (:node ((:id "b"))) (:node ((:id "c")))
                   (:edge ((:from "b") (:to "c"))))
                  (:cluster ((:id "c2") (:label "c2") (:fontname "Courier") (:style "dashed"))
                   (:node ((:id "d"))) (:node ((:id "e"))) (:node ((:id "f")))
                   (:edge ((:from "d") (:to "e")))
                   (:edge ((:from "e") (:to "f")))
                   (:edge ((:from "d") (:to "f"))))
                  (:node ((:id "g")))
                  (:edge ((:from "a") (:to "b")))
                  (:edge ((:from "a") (:to "d") (:lhead "c2") (:label "lhead='c2'")))
                  (:edge ((:from "b") (:to "e") (:ltail "c1")))
                  (:edge ((:from "d") (:to "c") (:ltail "c2")))
                  (:edge ((:from "a") (:to "g")))
                  (:edge ((:from "c") (:to "g") (:ltail "c1") (:label "ltail='c1'")))
                  (:edge ((:from "f") (:to "g") ))))
        (graph2 '(:graph ()
                  (:record ()
                   (:node ((:shape "ignored") (:id "10") (:label "left")))
                   (:node ((:id "(#(0 1 2 3 4) 5)") (:label "(#(0 1 2 3 4) 5)")))
                   (:node ((:id "12") (:label "right"))))
                  (:record ()
                   (:node ((:id "20") (:label "one")))
                   (:node ((:id "21") (:label "two"))))
                  (:record ()
                   (:node ((:id "30") (:label "{hello hello\\nworld}")))
                   (:record ()
                    (:node ((:id "311") (:label "b")))
                    (:record ()
                             (:node ((:id "3120") (:label "c")))
                             (:node ((:id "3121") (:label "d")))
                             (:node ((:id "3122") (:label "e"))))
                    (:node ((:id "313") (:label "f"))))
                   (:node ((:id "32") (:label "g")))
                   (:node ((:id "33") (:label "h"))))
                  (:edge ((:from "(#(0 1 2 3 4) 5)") (:to "20")))
                  (:edge ((:from "12") (:to "3121")))))
        (graph3 '(:graph ()
                  (:node ((:id "#(0 1)") (:label "#(0 1 2)")))
                  (:node ((:id "#(0 2)") (:label "#(0 2 2)")))
                  (:edge ((:from "#(0 2)") (:to "#(0 1)"))))))
    (render-s-dot (make-pathname :directory directory :name "test1" :type "gif") "gif" graph1)
    (render-s-dot (make-pathname :directory directory :name "test2" :type "ps") "ps" graph1)
    (render-s-dot (make-pathname :directory directory :name "test3" :type "jpg") "jpg" graph2)
    (render-s-dot (make-pathname :directory directory :name "test4" :type "png") "png" graph2)
    (render-s-dot (make-pathname :directory directory :name "test5" :type "svg") "svg" graph2)
    (render-s-dot (make-pathname :directory directory :name "test6" :type "ps") "ps" graph3)))

;;(test-s-dot)
