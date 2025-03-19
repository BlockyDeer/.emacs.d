;; 定义一个全局变量来存储用户设置的 Bash 命令
(defvar run-command-var nil
  "The Bash command to run when F7 is pressed.")

;; 设置 Bash 命令
(defun set-run-command (command)
  "Set the Bash command to be executed by F7."
  (interactive "sEnter the Bash command to run: ")
  (setq run-command-var command)
  (message "Run command set to: %s" command))

;; 运行 Bash 命令并显示在垂直分割的窗口中
(defun run-command-in-term ()
  "Run the Bash command stored in `my-run-command' in a vertically split terminal buffer."
  (interactive)
  (if run-command-var
      (progn
        ;; 垂直分割窗口
        (split-window-right)
        (other-window 1)
        ;; 创建一个新的终端缓冲区
        (let ((buffer (generate-new-buffer-name (concat "*Run: " run-command-var "*"))))
          (ansi-term "/bin/bash") ; 启动 Bash
          (rename-buffer buffer)
          ;; 发送命令到终端
          (term-send-string (get-buffer-process buffer) (concat run-command-var "\n"))))
    (message "No run command to be set. Use M-x set-run-command to set one.")))

(defun term-send-eof-and-close ()
  "Send EOF to the terminal process, close the window, and kill the buffer."
  (interactive)
  (when (term-check-proc (current-buffer))
    (term-send-eof))
  (if (y-or-n-p "Buffer has a running process; kill it?")
      (progn
        (kill-buffer (current-buffer))
        (delete-window)
    (message "Process not killed, buffer remains."))))

; C-d在能退出终端的时候清理掉Terminal buffer
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-d") 'term-send-eof-and-close)))

(global-set-key (kbd "<f7>") 'run-command-in-term)

(defvar run-commands-list '("./build/build.sh" "./run.sh")
  "List of commonly used bash commands.")

(defun select-run-command ()
  "Open a temporary buffer with common bash commands for selection."
  (interactive)
  (let ((buffer (get-buffer-create "*Run Command*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Pick the command you want to use as the one that runs your code or project.\n\n")
      (dolist (cmd run-commands-list)
        (insert (concat cmd "\n")))
      (goto-char (point-min))
      (read-only-mode t)
    (switch-to-buffer-other-window buffer)
    (local-set-key (kbd "RET") 'select-run-command-at-the-point)
    (local-set-key (kbd "q") 'close-select-bash-command-buffer))))

(defun select-run-command-at-the-point ()
  "Select the command at point and set it to `selected-command`."
  (interactive)
  (setq run-command-var (buffer-substring (line-beginning-position) (line-end-position)))
  (message "Selected command: %s" run-command-var)
  (close-select-bash-command-buffer))

(defun close-select-bash-command-buffer ()
  "Close the buffer and delete the window."
  (interactive)
  (let ((buffer (current-buffer)))
    (delete-window)
    (kill-buffer buffer)))

(global-set-key (kbd "C-c <f7>") 'select-run-command)

;; 提供 M-x run-command 来设置 Bash 命令
(defalias 'run-command 'set-run-command)

(provide 'run)
