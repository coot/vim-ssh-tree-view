" ---------------------
" - Editing files over ssh
"

fun! s:sshBufCreate()
  if bufname() =~ '^ssh:\/\/'
    set buftype=acwrite
  endif
endfun

fun! s:sshBufReadCmd()
  " While reading the file it is not modifiable.
  filetype detect
  set nomodifiable
  let b:sshView = winsaveview()
  if !exists("b:sshTempFile")
    let b:sshTempfile = tempname()
  endif
  let as = split(strpart(bufname(), 6), ':')
  let host = as[0]
  let file = join(as[1:])
  call job_start(
        \ ["ssh", host, "cat", file],
        \ {"exit_cb":  {handle, exitCode -> s:sshBufReadFn(host, file, bufnr(), handle, exitCode)},
        \  "out_io":   "file",
        \  "err_cb":   function("s:sshErrFn"),
        \  "out_name": b:sshTempfile} )
endfun

fun! s:sshBufReadFn(host, file, bufnr, handle, exitCode)
  if a:exitCode > 0
    set modifiable
    return
  endif
  if bufnr() != a:bufnr
    return
  endif
  set modifiable
  0,$d_
  exe "read " . b:sshTempfile
  1delete_
  if exists("b:sshView")
    call winrestview(b:sshView)
    unlet b:sshView
  endif
  set nomodified
endfun

fun! s:sshBufWriteCmd()
  if exists("b:sshWriteLock") && b:sshWriteLock
    return
  endif
  let b:sshWriteLock = v:true
  let tempfile = tempname()
  call delete(b:sshTempfile)
  exe "1,$write! " . b:sshTempfile
  call term_start(
        \ ["scp", b:sshTempfile, strpart(bufname(), 6)],
        \ {"hidden": v:true,
        \  "term_finish": "close",
        \  "exit_cb": function("s:sshBufWriteFn") })
endfun

fun! s:sshBufWriteFn(handler, exitCode)
  set nomodified
  if exists("b:sshWriteLock")
    unlet b:sshWriteLock
  endif
endfun

augroup SshEdit
  au!
  au BufCreate   ssh://* call s:sshBufCreate()
  au BufWriteCmd ssh://* call s:sshBufWriteCmd()
  au BufReadCmd  ssh://* call s:sshBufReadCmd()
augroup END

command -nargs=1 SshEdit :call SshEdit(<q-args>)

" -------------
" - Ssh Tree View
"

" ssh cache of directory structure
if !exists("g:sshCache")
  let g:sshCache = {}
endif

" insert directory into ssh cache
fun! s:sshCacheInsert(host, path, listing)
  " TODO: Use only host not host & username / credentials
  if has_key(g:sshCache, a:host)
    let dir = g:sshCache[a:host]
  else
    let g:sshCache[a:host] = { "contents": {} }
    let dir = g:sshCache[a:host]
  endif
  let pathComps = split(a:path, '/')
  let path=""
  for pathComp in pathComps
    let path = (empty(path) ? "" : path . "/") . pathComp
    if has_key(dir.contents, pathComp)
      let dir = dir.contents[pathComp]
    else
      let d = {"pathComp": pathComp,
            \  "path": path,
            \  "cached": v:false,
            \  "contents": {},
            \  "type": "/" }
      let dir.contents[pathComp] = d
      let dir = d
    endif
  endfor

  " cache invalidation is a non trivial problem!
  " let dir["contents"] = {}
  let dir.cached = v:true
  for typedPathComp in a:listing
    let pathComp = matchstr(typedPathComp, '.\{-}\ze[*/=>@|]\?$')
    if has_key(dir.contents, pathComp)
      continue
    endif
    let type = matchstr(typedPathComp, '[*/=>$|]$')
    if type == '/'
      let d = {"pathComp": pathComp,
            \  "path": (empty(a:path) ? "" : a:path . "/") . pathComp,
            \  "cached": v:false,
            \  "contents": {},
            \ "type": type }
      let dir["contents"][pathComp] = d
    else
      let f = {"pathComp": pathComp,
            \  "path": (empty(a:path) ? "" : a:path . "/") . pathComp,
            \  "cached": v:true,
            \  "type": type }
      let dir["contents"][pathComp] = f
    endif
  endfor
endfun

" find a path in `g:sshCache`
"
fun! s:sshCacheFind(host, path)
  if has_key(g:sshCache, a:host)
    let dir = g:sshCache[a:host]
  else
    let g:sshCache[a:host] = { "contents": {} }
    let dir = g:sshCache[a:host]
  endif
  let pathComps = split(a:path, '/')
  let path=""
  for pathComp in pathComps
    let path = (empty(path) ? "" : path . "/") . pathComp
    if has_key(dir["contents"], pathComp)
      let dir = dir["contents"][pathComp]
    else
      return v:null
    endif
  endfor
  return dir
endfun

" Open tree view
"
fun! s:sshTreeView(path) abort
  if a:path =~ ':'
    let args = split(a:path, ":")
    let host = args[0]
    let path = matchstr(get(args, 1, "~"), '^.\{-}\ze\/\?$')
  else
    let host = a:path
    let path = "~"
  endif
  let dir = s:sshCacheFind(host, path)
  if type(dir) != v:t_dict || (type(dir) == v:t_dict && !dir.cached)
    call s:openTreeViewAsync(host, path)
  else
    call s:openTreeViewSync(host, path)
  endif
endfun

command -nargs=1 -complete=custom,s:sshTreeViewComp SshTree :call s:sshTreeView(<q-args>)

fun! s:sshTreeViewComp(arg, cmdline, cursorpos)
  " TODO: use g:sshCache, record resutls in the cache
  let arg = matchstr(a:arg, '^\s*\zs.\{-}\ze\s*$')
  let args = split(arg, ':')
  let host = args[0]
  let path = len(args) >= 2 ? args[1] : ""
  let p = matchstr(path, '^.*\/')

  let res = split(system("ssh " . shellescape(host) . " ls -p1 " . shellescape(p))) 
  call filter(res, {idx, x -> x =~ '\/$'})
  call map(res, {idx, x -> host . ":" . (empty(p) ? x : p . x)})
  return join(res, "\n")
endfun

" The path must exists in `g:sshCache`
"
fun! s:openTreeViewSync(host, path) abort
  let path = (a:path =~ '^\/' || a:path =~ '^\~\/' || a:path == '~') ? a:path : "~/" . a:path
  if has_key(g:sshCache, a:host)
    let dir = g:sshCache[a:host]
  else
    echohl WarningMsg
    echom "No " . a:host
    echohl Normal
    return
  endif

  let bufs = tabpagebuflist()
  let bufnr = v:null
  for bnr in bufs
    if getbufvar(bnr, "SshTreeView", v:false)
      let bufnr = bnr
      break
    endif
  endfor
  if type(bufnr) == v:t_number
    let winnr = bufwinnr(bufnr)
    exe winnr . "wincmd w"
    %d_
  else
    vert 30 new
    let b:SshTreeView = v:true
    setlocal winfixwidth
    setlocal buftype=nofile
    setlocal bufhidden=wipe
    setlocal noswapfile
    setlocal nowrap
    setlocal nobuflisted
    setlocal nolist
    setlocal nofoldenable
    setlocal nonumber
    setlocal norelativenumber
    setf sshtreeview

    map <buffer> <silent> <Enter> :call <SID>treeViewEnter()<CR>
    map <buffer> <silent> C :call <SID>treeViewChangeRoot()<CR>
    map <buffer> <silent> u :call <SID>treeViewUp()<CR>
  endif

  let pathComps = split(path, '/')
  let path=""
  for pathComp in pathComps
    let path = path . "/" . pathComp
    if has_key(dir["contents"], pathComp)
      let dir = dir["contents"][pathComp]
    else
      let d = {"pathComp": pathComp,
            \  "path": path,
            \  "cached": v:false,
            \  "contents": {},
            \  "type": "directory" }
      let dir["contents"][pathComp] = d
      let dir = d
    endif
  endfor

  call append(line('$') - 1, a:host)
  call append(line('$') - 1, "..")
  call append(line('$') - 1, a:path)
  for pathComp in sort(keys(dir.contents))
    let node = dir.contents[pathComp]
    call append(line('$') - 1, (node.type == '/' ? '▸ ' : '  ') . node.pathComp)
  endfor
  call cursor(3, 1)
endfun

" If the `g:sshCache` does not contian inforamtion about the path, fetch it
" via ssh, add it to the cache and open tree view.
"
fun! s:openTreeViewAsync(host, path)
  let path = (a:path =~ '^\/' || a:path =~ '^\~\/' || a:path == '~') ? a:path : "~/" . a:path
  call job_start(
        \ ["ssh", a:host, "ls -1F", path],
        \ {"err_io": "pipe",
        \  "err_cb": function("s:sshErrFn"),
        \  "out_cb": {handle, msg -> s:openTreeViewAsyncFn(a:host, path, handle, msg)} })
endfun

fun! s:openTreeViewAsyncFn(host, path, handle, msg)
  let listing = split(a:msg, '\r\n')
  call s:sshCacheInsert(a:host, a:path, listing)
  call s:openTreeViewSync(a:host, a:path)
endfun

" ----------------
" - TreeView Utils
"

" List a directory on a remote server, and show the results in tree view.
"
fun! s:sshList(host, path, bufnr, line)
  let path = (a:path =~ '^\/' || a:path =~ '^\~\/' || a:path == '~') ? a:path : "~/" . a:path
  call job_start(
        \ ["ssh", a:host, "ls -1F", escape(path, ' ')],
        \ {"err_cb": function("s:sshErrFn"),
        \  "out_cb": {handle, msg -> s:sshListOutFn(a:host, path, handle, msg)},
        \  "exit_cb": {handle, exitCode -> s:sshListExitFn(a:host, path, handle, exitCode, a:bufnr, a:line)} })
endfun

fun! s:sshListOutFn(host, path, handle, msg)
  let listing = split(a:msg, '\r\n')
  call s:sshCacheInsert(a:host, a:path, listing)
endfun

fun! s:sshListExitFn(host, path, handle, errorCode, bufnr, line) abort
  let g:errorCode = a:errorCode
  if a:errorCode > 0
    return
  endif
  let wincmdp = v:false
  if a:bufnr != bufnr()
    let winnr = bufwinnr(a:bufnr)
    if winnr == -1
      return
    endif
    let wincmdp = v:true
    exe winnr . "wincmd w" 
  endif
  let lnr = 1
  while (getline(lnr) !=# a:line && lnr <= line("$"))
    let lnr += 1
  endwhile

  call setline(lnr, substitute(getline(lnr), '▸', '▾', ''))

  let indent = matchstr(getline(lnr), '^\s*') . "  "
  let idx = 0
  let dir = s:sshCacheFind(a:host, a:path)
  for pathComp in sort(keys(dir.contents))
    let e = dir.contents[pathComp]
    if e.type == '/'
      let lindent = indent . "▸ "
    else
      let lindent = indent . "  "
    endif
    call append(lnr + idx, lindent . e.pathComp)
    let idx += 1
  endfor

  if wincmdp
    wincmd p
  endif
endfun


"  Enter a directory in a tree view
"
fun! s:treeViewEnter() abort
  let line = line(".")
  if line == 1 || line == 3
    return
  endif
  if line == 2
    call s:treeViewUp()
    return
  endif
  let host=getline(1)
  let cwd=getline(3)
  let cline=getline(line("."))
  if cline =~ '^\s*▸ '
    let lnr = line(".")
    let host = getline(1)
    let path = s:treeViewCurrentPath()
    let dir = s:sshCacheFind(host, path)
    if type(dir) != v:t_dict || (type(dir) == v:t_dict && dir.cached == v:false)
      call s:sshList(host, path, bufnr(), getline(lnr))
    else
      let indent = substitute(matchstr(cline, '^\s*[▸▾] '), '.', ' ', 'g')
      let idx = 0
      for pathComp in sort(keys(dir.contents))
        if dir.contents[pathComp].type == '/'
          let lindent = indent . "▸ "
        else
          let lindent = indent . "  "
        endif
        call append(lnr + idx, lindent . dir.contents[pathComp].pathComp)
        let idx += 1
      endfor
      call setline(lnr, substitute(getline(lnr), '▸', '▾', ''))
    endif
  elseif cline =~ '^\s*▾ '
    let lnr = line(".")
    call setline(lnr, substitute(getline(lnr), '▾', '▸', ''))
    let indent = len(substitute(matchstr(getline(lnr), '^\s*[▸▾] '), '.', ' ', 'g'))
    while v:true
      let indent_ = len(substitute(matchstr(getline(lnr + 1), '^\s*\%([▸▾] \)\?'), '.', ' ', 'g'))
      if indent_ > indent
        exe "normal +dd_"
        normal -
      else
        break
      endif
    endwhile
  else
    let host = getline(1)
    let path = s:treeViewCurrentPath()
    wincmd p
    let bufname = "ssh://" . host . ":" . path
    let bufnr = bufnr(bufname)
    if bufnr != -1
      exe "b" . bufnr
    else
      exe "edit ssh://" . host . ":" . path
    endif
  endif
endfun

fun! s:treeViewChangeRoot()
  let path = s:treeViewCurrentPath()
  let host = getline(1)
  call s:sshTreeView(host . ":" . path)
endfun

fun! s:treeViewUp()
  let cwd = getline(3)
  let cwd_ = matchstr(cwd, '^.\{-}\ze\/[^/]*\/\?$')
  if cwd_ !=# cwd
    let host = getline(1)
    call s:sshTreeView(host . ":" . cwd_)
  endif
endfun

" Get path of the cursor postion.
"
fun! s:treeViewCurrentPath() abort
  let base = getline(3)
  let lnr = line(".")
  let l = getline(lnr)
  let as = matchlist(getline("."), '^\(\s*\%([▸▾] \)\?\)\(.*\)')
  let indent = len(substitute(as[1], '.', ' ', 'g'))
  let pathComps = [as[2]]

  while v:true
    let lnr = lnr - 1
    if lnr <= 3
      break
    endif
    let l = getline(lnr)
    let as = matchlist(l, '^\(\s*\%([▸▾] \)\?\)\(.*\)')
    let indent_ = len(substitute(as[1], '.', ' ', 'g'))
    if indent_ >= indent
      continue
    else
      let indent = indent_
      call add(pathComps, as[2])
    endif
    if indent <= 2
      break
    endif
  endwhile
  call add(pathComps, base)
  return join(reverse(pathComps), "/")
endfun

" -----------
" - Ssh utils
"

fun! s:sshErrFn(handle, msg)
  echohl Error
  echom a:msg
  echohl Normal
endfun

