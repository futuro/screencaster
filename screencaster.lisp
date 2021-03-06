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
    (winsize screencast-id display)
    ((:string "Window Size: ")
     (:string "Screencast ID: ")
     (:string "Display: "))
  "Start or stop a screencast"
  (let ((w-size (if *interactivep*
		    (car *command-hash*)
		    winsize))
	(cast-id (if *interactivep*
		       (caddr *command-hash*)
		       screencast-id))
	(x-display (if *interactivep*
		       (caddr *command-hash*)
		       display))
	(pulse-monitor (find-pulse-monitor))
	(pulse-mic (find-pulse-mic)))
    (if
     (equalp 0 (sb-ext::process-exit-code
		(sb-ext::run-program "/usr/bin/pgrep" (list "-f" "ffmpeg.*screencasts"))))
     (progn
       (message "Stopping screencast.")
       (sb-ext::run-program "/usr/bin/pkill" (list "-15" "-f" "ffmpeg.*screencasts")))

     (progn
       (message "~28<Beginning screencast.~>~%Video Geometry: ~A~%Screencast-Id: ~A~%X Display: ~A"
		w-size cast-id x-display)
       (run-shell-command
	(concatenate 'string
		     "ffmpeg -f x11grab -s " w-size
		     " -i " x-display
		     " -c:v libx264 -preset ultrafast "
		     " -an -map 0 ~/screencasts/video_" cast-id ".mp4 "
		     " -f pulse -i " pulse-monitor
		     " -c:a flac -vn -map 1 ~/screencasts/internal_" cast-id ".flac"
		     " -f pulse -i " pulse-mic
		     " -c:a flac -vn -map 2 ~/screencasts/mic_" cast-id ".flac"))))))

(defcommand quickcast () ()
  "Start a screencast with certain defaults"
  (let* ((win-geometry
	 (format nil "~Ax~A"
		 (screen-width (current-screen))
		 (screen-height (current-screen))))
	(date (multiple-value-bind
			(second minute hour date month year)
		      (get-decoded-time)
		    (format nil "~2,'0d-~2,'0d-~2,'0d_~2,'0d-~d-~d"
			    hour
			    minute
			    second
			    date
			    month
			    year)))
	 (cast-id (format nil "~A" date)))
    (screencast win-geometry cast-id ":0.0")))
