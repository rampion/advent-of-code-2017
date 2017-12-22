nnoremap <Leader>m :silent make bin/%:r \| redraw! \| cwindow \| cc<CR>
nnoremap <Leader>M :make bin/%:r<CR>
nnoremap <Leader>t :silent make test-%:r \| redraw! \| cwindow \| cc<CR>
nnoremap <Leader>T :make test-%:r<CR>
nnoremap <Leader>r :make run-%:r<CR>

set errorformat^=
      \%E###\ Failure\ in\ %f:%l:\ %m,
      \%+C\expected:\ %.%#,
      \%+C\ but\ got:\ %.%#,
      \%+C\ \ \ \ \ \ \ \ \ \ %.%#,
      \%-GExamples:\ %\\d%\\+\ \ Tried:\ %\\d%\\+\ \ Errors:\ %\\d%\\+\ \ Failures:\ %\\d%\\+
