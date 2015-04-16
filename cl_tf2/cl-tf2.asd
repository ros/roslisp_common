;;; Copyright (c) 2013, Georg Bartels <georg.bartels@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;; * Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;; * Neither the name of the Institute for Artificial Intelligence/
;;; Universitaet Bremen nor the names of its contributors may be used to
;;; endorse or promote products derived from this software without specific
;;; prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(defsystem "cl-tf2"
  :author "Georg Bartels <georg.bartels@cs.uni-bremen.de>"
  :license "BSD"
  :description "Common Lisp implementation of a TF2 client library."

  :depends-on (roslisp cl-transforms actionlib
                       tf2_msgs-msg geometry_msgs-msg std_msgs-msg
                       cl-tf-datatypes
                       cl-utils)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "errors" :depends-on ("package"))
             ;; (:file "datatypes" :depends-on ("package"))
             (:file "message-conversions" :depends-on ("package"
                                                       ;; "datatypes"
                                                       ))
             (:file "buffer-interface" :depends-on ("package"))
             (:file "utilities" :depends-on ("package" "errors"
                                                       ;; "datatypes"
                                                       "message-conversions"
                                                       "buffer-interface"))
             (:file "buffer-client" :depends-on ("package" "errors"
                                                           ;; "datatypes"
                                                           "message-conversions"
                                                           "buffer-interface"
                                                           "utilities"))
             (:file "transform-broadcaster" :depends-on ("package"
                                                         ;; "datatypes"
                                                         "message-conversions"))))))
