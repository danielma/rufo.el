(require 'f)

(defvar rufo-test/root-path (f-parent (f-parent (f-this-file))))

(require 'rufo (f-join rufo-test/root-path "rufo.el"))
