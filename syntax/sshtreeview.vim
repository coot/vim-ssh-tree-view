syn match SshTreeHost '\%1l.*'
syn match SshTreeUp '\%2l.*'
syn match SshTreeCWD '\%3l.*'
syn match SshTreeDirectory '[▸▾]\s.*'

highlight default link SshTreeHost Title
highlight default link SshTreeUp Directory
highlight default link SshTreeCWD Statement
highlight default link SshTreeDirectory Directory
