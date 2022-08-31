;;; sl-cpp-root.el --- extern the ede-cpp-root-project slot
;;; Commentary:
;;; Code:

(require 'ede/cpp-root)

(defclass sl-ede-cpp-root-project (ede-cpp-root-project)
  ((source-path :initarg :source-path
		 :initform '( "/source" "../source/" )
		 :type list
		 :documentation
		 "The source filenames within a project.
The `ede-cpp-root-project' provied the :include-path to
optimizate the search in large projects, that's enough
for get function declaration info, but sometimes we want
to read the function definition, The `:source-path' give
the hints for tags tools (eg: cscope, global) to do that.

Directories that are relative to the project's root
should start with a /, such as  \"/source\", meaning the
directory `source' off the project root directory."
		 )
   (exclude-path :initarg :exclude-path
		 :initform '( "/exclude" "../exclude/" )
		 :type list
		 :documentation
		 "The exclude path in a project.
The `ede-cpp-root-project' provied the :include-path to
optimizate the search in large projects, when some times
we want ignore some directories in deep file structure,
The `:exclude-path' give the hits for tags tools
(eg: cscope, global) to do that.

The `exclude-path' can be a regexp within `find' style."
		 )
   )
  "SL EDE cpp-root project class.
Each directory needs a project file to control it.")

(cl-defmethod initialize-instance ((this sl-ede-cpp-root-project)
				&rest fields)
  "Make sure the :file is fully expanded."
  ;; Add ourselves to the master list
  (cl-call-next-method))

(provide 'sl-cpp-root)
;;; sl-cpp-root ends here
