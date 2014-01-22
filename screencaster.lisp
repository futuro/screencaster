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
  (let ((win-geometry
	 (format nil "~Ax~A"
		 (window-width (current-window))
		 (window-height (current-window))))
	(filename (multiple-value-bind
			(second minute hour date month year)
		      (get-decoded-time)
		    (format nil "cast_~2,'0d:~2,'0d:~2,'0d_~2,'0d-~d-~d.mkv"
			    hour
			    minute
			    second
			    date
			    month
			    year))))
    (screencast win-geometry "ultrafast" filename ":0.0")))
