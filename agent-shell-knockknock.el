;;; agent-shell-knockknock.el --- Knockknock notifications for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alvaro Ramirez

;; Author: Alvaro Ramirez
;; URL: https://github.com/xenodium/agent-shell-knockknock
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (agent-shell "0.1") (knockknock "0.1"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Display knockknock notifications for agent-shell events like
;; permission requests and turn completions.

;;; Code:

(require 'agent-shell)
(require 'knockknock)
(require 'map)

(defvar agent-shell-knockknock-duration 5
  "Duration in seconds for notifications.")

(defvar agent-shell-knockknock--subscriptions nil
  "Alist mapping shell buffers to their subscription tokens.")

(defvar agent-shell-knockknock--temp-binding-timer nil
  "Timer to deactivate the temporary RET binding.")

(defun agent-shell-knockknock--shell-visible-p (shell-buffer)
  "Return non-nil if SHELL-BUFFER or its viewport buffer is visible."
  (or (get-buffer-window shell-buffer t)
      (when-let ((viewport (agent-shell-viewport--buffer
                            :shell-buffer shell-buffer
                            :existing-only t)))
        (get-buffer-window viewport t))))

(defun agent-shell-knockknock--icon-file (shell-buffer)
  "Get the agent icon file path for SHELL-BUFFER."
  (when-let* ((state (buffer-local-value 'agent-shell--state shell-buffer))
              (icon-name (map-nested-elt state '(:agent-config :icon-name))))
    (agent-shell--fetch-agent-icon icon-name)))

(defun agent-shell-knockknock--strip-kind-prefix (text kind)
  "Strip KIND prefix from TEXT to avoid redundancy.
For example, \"Edit README.org\" with kind \"edit\" becomes \"README.org\"."
  (if (and kind text
           (string-match-p (concat "\\`" (regexp-quote kind) " ")
                           (downcase text)))
      (string-trim (substring text (length kind)))
    text))

(defun agent-shell-knockknock--format-permission-message (tool-call)
  "Format a user-friendly message from TOOL-CALL."
  (let ((stripped (agent-shell-knockknock--strip-kind-prefix
                   (map-elt tool-call :title)
                   (map-elt tool-call :kind))))
    (pcase (map-elt tool-call :kind)
      ((or "read" "edit" "write" "delete" "move")
       (agent-shell--shorten-paths stripped))
      ((or "execute" "search" "fetch")
       (let ((first-line (car (split-string stripped "\n"))))
         (if (> (length first-line) 50)
             (concat (substring first-line 0 47) "...")
           first-line)))
      (_ stripped))))

(defun agent-shell-knockknock--switch-to-shell (shell-buffer)
  "Switch to SHELL-BUFFER or its viewport and close the notification."
  (when (buffer-live-p shell-buffer)
    (switch-to-buffer
     (or (agent-shell-viewport--buffer
          :shell-buffer shell-buffer
          :existing-only t)
         shell-buffer)))
  (knockknock-close))

(defun agent-shell-knockknock--install-ret-binding (shell-buffer)
  "Install a transient RET binding to switch to SHELL-BUFFER.
The binding auto-removes after `agent-shell-knockknock-duration' seconds."
  (when agent-shell-knockknock--temp-binding-timer
    (cancel-timer agent-shell-knockknock--temp-binding-timer))
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
                (lambda ()
                  (interactive)
                  (when agent-shell-knockknock--temp-binding-timer
                    (cancel-timer agent-shell-knockknock--temp-binding-timer)
                    (setq agent-shell-knockknock--temp-binding-timer nil))
                  (agent-shell-knockknock--switch-to-shell shell-buffer)))
    (setq agent-shell-knockknock--temp-binding-timer
          (run-with-timer agent-shell-knockknock-duration nil
                          (lambda ()
                            (setq agent-shell-knockknock--temp-binding-timer nil))))
    (set-transient-map map (lambda () agent-shell-knockknock--temp-binding-timer))))

(defun agent-shell-knockknock--on-permission-request (event)
  "Handle a permission-request EVENT with a knockknock notification."
  (unless (agent-shell-knockknock--shell-visible-p (current-buffer))
    (knockknock-notify
     :title (capitalize (or (map-nested-elt event '(:data :tool-call :kind)) ""))
     :message (agent-shell-knockknock--format-permission-message
               (map-nested-elt event '(:data :tool-call)))
     :icon-file (agent-shell-knockknock--icon-file (current-buffer))
     :duration agent-shell-knockknock-duration)
    (agent-shell-knockknock--install-ret-binding (current-buffer))))

(defun agent-shell-knockknock--on-turn-complete (event)
  "Handle a turn-complete EVENT with a knockknock notification."
  (unless (agent-shell-knockknock--shell-visible-p (current-buffer))
    (knockknock-notify
     :title "Finished"
     :message (if (equal (map-nested-elt event '(:data :stop-reason))
                         "end_turn")
                  "Success"
                "Failed")
     :icon-file (agent-shell-knockknock--icon-file (current-buffer))
     :duration agent-shell-knockknock-duration)
    (agent-shell-knockknock--install-ret-binding (current-buffer))))

(defun agent-shell-knockknock-subscribe (&optional shell-buffer)
  "Subscribe to agent-shell events in SHELL-BUFFER.
If SHELL-BUFFER is nil, use the current buffer."
  (let ((buf (or shell-buffer (current-buffer))))
    (when (map-elt agent-shell-knockknock--subscriptions buf)
      (agent-shell-knockknock-unsubscribe buf))
    (setf (map-elt agent-shell-knockknock--subscriptions buf)
          (list
           (agent-shell-subscribe-to
            :shell-buffer buf
            :event 'permission-request
            :on-event #'agent-shell-knockknock--on-permission-request)
           (agent-shell-subscribe-to
            :shell-buffer buf
            :event 'turn-complete
            :on-event #'agent-shell-knockknock--on-turn-complete)))))

(defun agent-shell-knockknock-unsubscribe (&optional shell-buffer)
  "Unsubscribe from agent-shell events in SHELL-BUFFER.
If SHELL-BUFFER is nil, use the current buffer."
  (let ((buf (or shell-buffer (current-buffer))))
    (dolist (token (map-elt agent-shell-knockknock--subscriptions buf))
      (agent-shell-unsubscribe :subscription token))
    (setq agent-shell-knockknock--subscriptions
          (map-delete agent-shell-knockknock--subscriptions buf))))

;;;###autoload
(define-minor-mode agent-shell-knockknock-mode
  "Toggle knockknock notifications for the current agent-shell buffer."
  :lighter " knock"
  (if agent-shell-knockknock-mode
      (agent-shell-knockknock-subscribe (current-buffer))
    (agent-shell-knockknock-unsubscribe (current-buffer))))

(provide 'agent-shell-knockknock)

;;; agent-shell-knockknock.el ends here
