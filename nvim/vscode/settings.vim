
"VSCode
"function s:split(...) abort
"	 let direction = a:1
"	 let file = a:2
"	 call VSCodeCall(direction == 'h' ? "workbench.action.splitEditorDown' : 'workbench.action.splitEditorRight')
"	 if file !=''
"		call VSCodeExtensionNotify('open-file', expand(file), 'all')
"	 endif
"endfunction
"
"
"function s:splitNew(...)
"	let file = a:2
"	call s:split(a:1, file == '' ? '__vscode_new__' : file)
"endfunction
"
"function s:closeOtherEditors()
"	call VSCodeNotify('workbench.action.closeEditorsInOtherGroups')
"	call VSCodeNotify('workbench.action.closeOtherEditors')
"endfunction
"
"function! s:manageEditorSize(...)
"	let count = a:1
"	let to = a:2
"	for i in range(1, count ? count : 1)
"		call VSCodeNotify(to == 'increase' ? 'workbench.action.increaseViewSize' : 'workbench.action.decreaseViewSize')
"	endfor
"endfunction 
"
"
"
"command! -complete=file -nargs=? Split call <SID>splist('h', <q-args>)
"command! -complete=file -nargs=? Vsplit call <SID>splist('v', <q-args>)
"command! -complete=file -nargs=? New call <SID>splist('h', '__vscode_new__')
"command! -complete=file -nargs=? Vnew call <SID>splist('v', '__vscode_new__')
"command! -bang Only if <q-bang> == '!' | call <SID>closeOtherEditors() | else | call VSCodeNotify('workbench.action.joinAllGroups') | endif
"
"nnoremap <silent> <C-w>s :call <SID>split('h')<CR>
"xnoremap <silent> <C-w>s :call <SID>split('h')<CR>
"		
"nnoremap <silent> <C-w>v :call <SID>split('v')<CR>
"xnoremap <silent> <C-w>v :call <SID>split('v')<CR>
"
"nnoremap <silent> <C-w>v :call <SID>splitNew('h', '__vscode_new__')<CR>
"xnoremap <silent> <C-w>v :call <SID>splitNew('h', '__vscode_new__')<CR>


" Better Navigation
nnoremap <silent> <C-j> :call VSCodeNotify('workbench.action.navigateDown')<CR>
xnoremap <silent> <C-j> :call VSCodeNotify('workbench.action.navigateDown')<CR>

nnoremap <silent> <C-k> :call VSCodeNotify('workbench.action.navigateUp')<CR>
xnoremap <silent> <C-k> :call VSCodeNotify('workbench.action.navigateUp')<CR>

nnoremap <silent> <C-h> :call VSCodeNotify('workbench.action.navigateLeft')<CR>
xnoremap <silent> <C-h> :call VSCodeNotify('workbench.action.navigateLeft')<CR>

nnoremap <silent> <C-l> :call VSCodeNotify('workbench.action.navigateRight')<CR>
xnoremap <silent> <C-l> :call VSCodeNotify('workbench.action.navigateRight')<CR>

" Bind C-/ to vscode commentary since calling from vscode produce double
" comments
xnoremap <silent> <C-/> :call Comment()<CR>
nnoremap <silent> <C-/> :call Comment()<CR>

nnoremap <silent> <C-w>_ :<C-u>call VSCodeNotify('workbench.action.toggleEditorWidth')<CR

xnoremap <silent> <Space> :call VSCodeNotify('whichkey.show')<CR>
nnoremap <silent> <Space> :call VSCodeNotify('whichkey.show')<CR>

"function! VSCodeNotifyVisual(cmd, leaveSelection, ...)
"    let mode = mode()
"    if mode ==# 'V'
"        let startLine = line('v')
"        let endLine = line('.')
"        call VSCodeNotifyRange(a:cmd, startLine, endLine, a:leaveSelection, a:000)
"    elseif mode ==# 'v' || mode ==# "\<C-v>"
"        let startPos = getpos('v')
"        let endPos = getpos('.')
"        call VSCodeNotifyRangePos(a:cmd, startPos[1], endPos[1], startPos[2], endPos[2] + 1, a:leaveSelection, a:000)
"    else
"        call VSCodeNotify(a:cmd, a:000)
"    endif
"endfunction


" xnoremap <C-S-P> <Cmd>call VSCodeNotifyVisual('workbench.action.showCommands', 1)<CR>
" nnoremap <C-w>gd <Cmd>call VSCodeNotify('editor.action.revealDefinitionAside')<CR>
" nnoremap ? <Cmd>call VSCodeNotify('workbench.action.findInFiles', { 'query': expand('<cword>')})<CR>
