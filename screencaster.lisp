;; Helper commands to make screencasts with ffmpeg and stumpwm
;;
;; Maintainer: Futuro
;; License: GPLv3

(in-package :stumpwm)

(defun find-pulse-monitor ()
  "Find the first monitor pulse audio defines."
  ;; XXX I have no idea if this is robust or not, but it probably isn't
  (string-trim '(#\Newline)
	       (run-shell-command "/usr/bin/pactl list sources |awk '/Name.*monitor/ {print $2}'" t)))

(defun find-pulse-mic ()
  "Find the first mic pulse audio defines."
  ;; XXX I have no idea if this is robust or not, but it probably isn't
  (string-trim '(#\Newline)
	       (run-shell-command
		"/usr/bin/pactl list sources |awk '(/Name.*/ && $0 !~ /Name.*monitor/) {print $2}'"
		t)))

(defcommand screencast
    (winsize preset filename display)
    ((:string "Window Size: ")
     (:string "FFMPeg preset: ")
     (:string "Filename: ")
     (:string "Display: "))
  "Start or stop a screencast"
  (let ((w-size (if *interactivep*
		    (car *command-hash*)
		    winsize))
	(vid-preset (if *interactivep*
			(cadr *command-hash*)
			preset))
	(cast-name (if *interactivep*
		       (caddr *command-hash*)
		       filename))
	(x-display (if *interactivep*
		       (caddr *command-hash*)
		       display)))
    (if
     (equalp 0 (sb-ext::process-exit-code
		(sb-ext::run-program "/usr/bin/pgrep" (list "-f" "ffmpeg.*screencasts"))))
     (progn
       (message "Stopping screencast.")
       (sb-ext::run-program "/usr/bin/pkill" (list "-15" "-f" "ffmpeg.*screencasts")))

     (progn
       (message "~28<Beginning screencast.~>~%Video Geometry: ~A~%Video Preset: ~A~%Filename: ~A~%X Display: ~A"
		w-size vid-preset cast-name x-display)
       (run-shell-command
	(concatenate 'string
		     "ffmpeg -f x11grab -s " w-size
		     " -i " x-display " -f alsa -i pulse "
		     " -c:v libx264 -preset " vid-preset
		     " -c:a libvorbis ~/screencasts/" cast-name))))))

(defcommand quickcast () ()
  "Start a screencast with certain defaults"
  (let* ((win-geometry
	 (format nil "~Ax~A"
		 (screen-width (current-screen))
		 (screen-height (current-screen))))
	(date (multiple-value-bind
			(second minute hour date month year)
		      (get-decoded-time)
		    (format nil "~2,'0d:~2,'0d:~2,'0d_~2,'0d-~d-~d.mkv"
			    hour
			    minute
			    second
			    date
			    month
			    year))
	  (filename (format nil "screen_~A" date))))
    (screencast win-geometry "ultrafast" filename ":0.0")))
