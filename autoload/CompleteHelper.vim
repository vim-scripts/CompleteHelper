" CompleteHelper.vim: Generic functions to support custom insert mode completions.
"
" DEPENDENCIES:
"   - ingo/compat.vim autoload script
"   - ingo/list.vim autoload script
"   - ingo/pos.vim autoload script
"   - ingo/text.vim autoload script
"   - CompleteHelper/Abbreviate.vim autoload script
"
" Copyright: (C) 2008-2014 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'.
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS
"   1.50.027	18-Dec-2014	ENH: Add a:options.abbreviate and evaluate in
"				CompleteHelper#AddMatch(). This saves completion
"				plugins from doing an additional map() over the
"				List of matches.
"   1.50.026	27-Nov-2014	Split the match extraction via pattern match
"				from the window / buffer iteration, which now
"				takes a generic Funcref, allowing for other
"				algorithms: Remove ...Matches... from the
"				s:FindMatchesIn... functions. Extract
"				s:MatchInCurrent() and s:MatchInBuffer().
"				Add CompleteHelper#Find() generic alternative to
"				CompleteHelper#FindMatches() that takes a
"				Funcref instead of a regular expression.
"				Expose CompleteHelper#AddMatch().
"   1.42.025	29-Jul-2014	getbufline() can only access loaded buffers, for
"				completion from unloaded buffers, we need to use
"				readfile().
"   1.41.024	30-Apr-2014	FIX: In the completion buffer, check for the
"				cursor position being anywhere in the match, not
"				just at the end. We must not only avoid matching
"				the base, but any text around the cursor. This
"				is especially important for completion repeats,
"				to avoid offering text after the cursor.
"   1.40.023	03-Apr-2014	Allow to debug the pattern via :let
"				g:CompleteHelper_DebugPatterns = [].
"   1.33.022	17-Jan-2014	Check for existence of 'autochdir'.
"   1.33.021	07-Jan-2014	FIX: a:options.backward_search with falsy value
"				also enables backward search.
"				Add g:CompleteHelper_IsDefaultToBackwardSearch
"				config var that lets
"				CompleteHelper#FindMatches() default to
"				backwards search when no
"				a:options.backward_search is given. Since all of
"				my custom completions don't offer separate
"				backward / forward mappings, and backward search
"				(i.e. offering first what got recently typed)
"				makes more sense, default to backward search
"				from now on.
"   1.33.020	18-Dec-2013	Remove the duplicated implementation in
"				CompleteHelper#ExtractText(), deprecate it, and
"				delegate to ingo#text#Get().
"   1.32.019	15-Oct-2013	Replace conditional with ingo#list#Make().
"   1.32.018	02-Oct-2013	ENH: Allow to pass a List of regular expressions
"				to CompleteHelper#FindMatches(). If you have
"				multiple regular expressions that can match at
"				the same position and should yield separate
"				matches, you cannot use regular expression
"				branches.
"   1.32.017	08-Aug-2013	Move escapings.vim into ingo-library.
"   1.31.016	07-Mar-2013	Avoid "E11: Invalid in command-line window"
"				error when performing completions that search
"				other windows from the command-line window. Use
"				the buffer-search instead; it does not need to
"				change the current window for its search.
"				FIX: Don't abort iteration of buffers in
"				s:FindMatchesInOtherBuffers() when one buffer
"				was already searched; instead :continue with the
"				next.
"   1.30.015	27-Sep-2012	Optimization: Skip search in other windows where
"				there's only one that got searched already by
"				s:FindMatchesInCurrentWindow().
"				Optimization: Only visit window when its buffer
"				wasn't already searched.
"				ENH: Allow skipping of buffers via new
"				a:options.bufferPredicate Funcref.
"   1.20.014	03-Sep-2012	ENH: Implement a:options.complete = 'b' (only
"				supporting single-line matches and no
"				a:options.extractor).
"				Factor out s:AddMatch().
"				Transparently handle 'autochdir': still show the
"				correct relative path in matches from other
"				windows, and restore the buffer's CWD even if it
"				was temporarily changed.
"   1.11.013	01-Sep-2012	Make a:matchObj in CompleteHelper#ExtractText()
"				optional; it's not used there, anyway. This
"				avoids having to pass an empty dictionary just
"				to satisfy the API.
"				Introduce a:alreadySearchedBuffers to allow for
"				swapped order in a:options.complete and to
"				prepare for additional complete options.
"   1.10.012	04-May-2012	Factor out CompleteHelper#Abbreviate#Text() to
"				allow processing of completion menu text, too.
"   1.00.011	31-Jan-2012	Prepare for publish.
"	010	04-Oct-2011	Turn multi-line join into
"				CompleteHelper#JoinMultiline() utility function
"				and remove the default processing, now that I
"				have found a workaround to make Vim handle
"				matches with newlines. Rename "multiline" option
"				to a convenience "processor" option, to be used
"				by LongestComplete.vim.
"	009	04-Oct-2011	Move CompleteHelper#Abbreviate() from
"				MotionComplete.vim to allow reuse.
"				Also translate newline characters.
"	008	04-Mar-2010	Collapse multiple lines consisting of only
"				whitespace and a newline into a single space,
"				not one space per line.
"	007	25-Jun-2009	Now using :noautocmd to avoid unnecessary
"				processing while searching other windows.
"	006	09-Jun-2009	Do not include a match ending at the cursor
"				position when finding completions in the buffer
"				where the completion is undertaken.
"				Vim would not offer this anyway, and this way it
"				feels cleaner and does not confuse unit tests.
"				Such a match can happen if a:base =~ a:pattern.
"	005	03-Mar-2009	Now restoring window sizes in
"				s:FindMatchesInOtherWindows() to avoid
"				increating window height from 0 to 1.
"	004	19-Aug-2008	Initial matchObj is now passed to text extractor
"				function.
"	003	18-Aug-2008	Added a:options.multiline; default is to
"				collapse newline and surrounding whitespace into
"				a single <Space>.
"	002	17-Aug-2008	BF: Check for match not yet in the list still
"				used match text, not object.
"	001	13-Aug-2008	file creation
let s:save_cpo = &cpo
set cpo&vim

if ! exists('g:CompleteHelper_IsDefaultToBackwardSearch')
    let g:CompleteHelper_IsDefaultToBackwardSearch = 1
endif

function! s:ShouldBeSearched( options, bufnr )
    return ! has_key(a:options, 'bufferPredicate') || call(a:options.bufferPredicate, [a:bufnr])
endfunction
function! CompleteHelper#ExtractText( startPos, endPos, ... )
"*******************************************************************************
"* DEPRECATED:
"   Use ingo#text#Get() instead.
"*******************************************************************************
    return ingo#text#Get(a:startPos, a:endPos)
endfunction
function! CompleteHelper#AddMatch( matches, matchObj, matchText, options )
    let l:matchText = a:matchText

    " Custom processing of match text.
    if has_key(a:options, 'processor')
	let l:matchText = a:options.processor(l:matchText)
    endif

    " Store match text in match object.
    let a:matchObj.word = l:matchText

    if get(a:options, 'abbreviate', 0)
	call CompleteHelper#Abbreviate#Word(a:matchObj)
    endif

    " Only add if this is an actual match that is not yet in the list of
    " matches.
    if ! empty(l:matchText) && index(a:matches, a:matchObj) == -1
	call add(a:matches, a:matchObj)
    endif
endfunction
function! s:MatchInCurrent( lines, matches, matchTemplate, options, isInCompletionWindow )
    let l:isBackward = (has_key(a:options, 'backward_search') ?
    \   a:options.backward_search :
    \   g:CompleteHelper_IsDefaultToBackwardSearch
    \)

    let l:save_cursor = getpos('.')

    for l:pattern in s:patterns
	let l:firstMatchPos = [0,0]
	while ! complete_check()
	    let l:matchPos = searchpos( l:pattern, 'w' . (l:isBackward ? 'b' : '') )
	    if l:matchPos == [0,0] || l:matchPos == l:firstMatchPos
		" Stop when no matches or wrapped around to first match.
		break
	    endif
	    if l:firstMatchPos == [0,0]
		" Record first match position to detect wrap-around.
		let l:firstMatchPos = l:matchPos
	    endif

	    let l:matchEndPos = searchpos( l:pattern, 'cen' )
	    if a:isInCompletionWindow && ingo#pos#IsInside(l:save_cursor[1:2], l:matchPos, l:matchEndPos)
		" Do not include a match around the cursor position; this would
		" either just return the completion base, which Vim would not
		" offer anyway, or the completion base and following text, which
		" is unlikely to be desired, and not offered by the built-in
		" completions, neither. By avoiding this match, we may shrink
		" down the completion list to a single match, which would be
		" inserted immediately without the user having to choose one.
		continue
	    endif

	    " Initialize the match object and extract the match text.
	    let l:matchObj = copy(a:matchTemplate)
	    let l:matchText = (has_key(a:options, 'extractor') ? a:options.extractor(l:matchPos, l:matchEndPos, l:matchObj) : ingo#text#Get(l:matchPos, l:matchEndPos))

	    call CompleteHelper#AddMatch(a:matches, l:matchObj, l:matchText, a:options)
"****D echomsg '**** completion triggered from' string(l:save_cursor[1:2])
"****D echomsg '**** match in' . (a:isInCompletionWindow ? ' current' : '') 'buffer' bufnr('') 'from' string(l:matchPos) 'to' string(l:matchEndPos) string(l:matchText)
	endwhile

	call setpos('.', l:save_cursor)
    endfor
endfunction
function! s:FindInCurrentWindow( alreadySearchedBuffers, matches, Funcref, matchTemplate, options, isInCompletionWindow )
    if has_key(a:alreadySearchedBuffers, bufnr(''))
	return
    endif
    let a:alreadySearchedBuffers[bufnr('')] = 1
    if ! s:ShouldBeSearched(a:options, bufnr(''))
	return
    endif

    call call(a:Funcref, [[], a:matches, a:matchTemplate, a:options, a:isInCompletionWindow])
endfunction
function! s:FindInOtherWindows( alreadySearchedBuffers, matches, Funcref, options )
    let l:originalWinNr = winnr()
    if winnr('$') == 1 && has_key(a:alreadySearchedBuffers, winbufnr(l:originalWinNr))
	" There's only one window, and we have searched it already (probably via s:FindInCurrentWindow()).
	return
    endif

    " By entering a window, its height is potentially increased from 0 to 1 (the
    " minimum for the current window). To avoid any modification, save the window
    " sizes and restore them after visiting all windows.
    let l:originalWindowLayout = winrestcmd()

    " Unfortunately, restoring the 'autochdir' option clobbers any temporary CWD
    " override. So we may have to restore the CWD, too.
    let l:save_cwd = getcwd()
    let l:chdirCommand = (haslocaldir() ? 'lchdir!' : 'chdir!')

    " The 'autochdir' option adapts the CWD, so any (relative) filepath to the
    " filename in the other window would be omitted. Temporarily turn this off;
    " may be a little bit faster, too.
    if exists('+autochdir')
	let l:save_autochdir = &autochdir
	set noautochdir
    endif

    try
	for l:winNr in range(1, winnr('$'))
	    if ! has_key(a:alreadySearchedBuffers, winbufnr(l:winNr)) && s:ShouldBeSearched(a:options, winbufnr(l:winNr))
		execute 'noautocmd' l:winNr . 'wincmd w'

		let l:matchTemplate = {'menu': bufname('')}
		call s:FindInCurrentWindow(a:alreadySearchedBuffers, a:matches, a:Funcref, l:matchTemplate, a:options, 0)
	    endif
	endfor
    finally
	execute 'noautocmd' l:originalWinNr . 'wincmd w'
	silent! execute l:originalWindowLayout

	if exists('l:save_autochdir')
	    let &autochdir = l:save_autochdir
	endif
	if getcwd() !=# l:save_cwd
	    execute l:chdirCommand ingo#compat#fnameescape(l:save_cwd)
	endif
    endtry
endfunction
function! s:GetListedBufnrs()
    return filter(
    \   range(1, bufnr('$')),
    \   'buflisted(v:val)'
    \)
endfunction
function! s:GetBufferLines( bufnr )
    if bufloaded(a:bufnr)
	return getbufline(a:bufnr, 1, '$')
    else
	" getbufline() can only access loaded buffers, for unloaded ones, we
	" need to use readfile(). This has the downside of not considering the
	" file's encoding, but Vim's built-in completion (in version 7.4.316)
	" doesn't, neither (presumably because it also uses readfile()).
	try
	    return readfile(bufname(a:bufnr))
	catch /^Vim\%((\a\+)\)\=:E484/ " E484: Can't open file
	    return []
	endtry
    endif
endfunction
function! s:MatchInBuffer( lines, matches, matchTemplate, options, isInCompletionWindow )
    for l:pattern in s:patterns
	for l:line in a:lines
	    " Note: Do not just use matchstr() with {count}, because we cannot
	    " reliably recognize whether an empty result just means "empty match
	    " at {count}" or actually means "no more matches".
	    let l:endPos = 0
	    while 1
		let l:startPos = l:endPos
		let l:endPos = matchend(l:line, l:pattern, l:startPos)
		if l:endPos == -1
		    break
		endif

		call CompleteHelper#AddMatch(a:matches, copy(a:matchTemplate), matchstr(l:line, l:pattern, l:startPos), a:options)
	    endwhile
	endfor

	if complete_check()
	    break
	endif
    endfor
endfunction
function! s:FindInOtherBuffers( alreadySearchedBuffers, matches, Funcref, options, bufnrs )
    for l:bufnr in a:bufnrs
	if has_key(a:alreadySearchedBuffers, l:bufnr)
	    continue
	endif
	let a:alreadySearchedBuffers[l:bufnr] = 1
	if ! s:ShouldBeSearched(a:options, l:bufnr)
	    continue
	endif

	let l:matchTemplate = {'menu': bufname(l:bufnr)}

	" We need to get all lines at once; there is no other way to remotely
	" determine the number of lines in the other buffer.
	call call(a:Funcref, [s:GetBufferLines(l:bufnr), a:matches, l:matchTemplate, a:options, 0])
    endfor
endfunction
function! CompleteHelper#FindMatches( matches, pattern, options )
"*******************************************************************************
"* PURPOSE:
"   Find matches for a:pattern according to a:options and store them in
"   a:matches.
"* ASSUMPTIONS / PRECONDITIONS:
"   none
"* EFFECTS / POSTCONDITIONS:
"   none
"* INPUTS:
"   a:matches	(Empty) List that will hold the matches (in Dictionary format,
"		cp. :help complete-functions). Matches will be appended.
"   a:pattern	Regular expression specifying what text will match as a
"		completion candidate.
"		If you have multiple regular expressions that can match at the
"		same position and should yield separate matches, you cannot use
"		regular expression branches. Instead, pass a List of regular
"		expressions for a:pattern.
"		Note: In the buffer where the completion takes place, Vim
"		temporarily removes the a:base part (as passed to the
"		complete-function) during the completion. This helps avoiding
"		that the text directly after the cursor also matches a:pattern
"		(assuming something like '\<'.a:base.'\k\+') and appears in the
"		list.
"		Note: Matching is done via the searchpos() function, so the
"		'ignorecase' and 'smartcase' settings apply. Add |/\c| / |/\C|
"		to the regexp to set the case sensitivity.
"		Note: An empty pattern does not match at all, so take care of
"		passing a sensible default! '\V' will match every single
"		character individually; probably not what you want.
"		Note: for a:options.complete = 'b', matching is limited to
"		within single lines.
"   a:options	Dictionary with match configuration:
"   a:options.complete	    Specifies what is searched, like the 'complete'
"			    option. Supported options: '.' for current buffer,
"			    'w' for buffers from other windows, 'b' for other
"			    loaded buffers from the buffer list.
"   a:options.backward_search	Flag whether to search backwards from the cursor
"				position.
"   a:options.extractor	    Funcref that extracts the matched text from the
"			    current buffer. Will be invoked with ([startLine,
"			    startCol], [endLine, endCol], matchObj) arguments
"			    with the cursor positioned at the start of the
"			    current match; must return string; can modify the
"			    initial matchObj.
"			    Note: Is not used for a:options.complete = 'b'.
"   a:options.processor	    Funcref that processes matches. Will be invoked with
"			    an a:matchText argument; must return processed
"			    string, or empty string if the match should be
"			    discarded. Alternatively, you can filter() / map()
"			    the a:matches result returned from this function,
"			    but passing in a function may be easier for you (and
"			    may avoid that a lot of duplicates consume memory
"			    unnecessarily).
"   a:options.bufferPredicate   Funcref that decides whether a particular buffer
"				should be searched. It is passed a buffer number
"				and must return 0 when the buffer should be
"				skipped.
"   a:options.abbreviate        Automatically abbreviate each match with
"				CompleteHelper#Abbreviate#Word().
"* RETURN VALUES:
"   a:matches
"*******************************************************************************
    let l:complete = get(a:options, 'complete', '')
    let s:patterns = ingo#list#Make(a:pattern)
    if exists('g:CompleteHelper_DebugPatterns')
	if type(g:CompleteHelper_DebugPatterns) == type([])
	    let g:CompleteHelper_DebugPatterns = s:patterns
	else
	    echomsg '****' string(s:patterns)
	endif
    endif
    let l:searchedBuffers = {}
    for l:places in split(l:complete, ',')
	if l:places ==# '.'
	    call s:FindInCurrentWindow(l:searchedBuffers, a:matches, function('s:MatchInCurrent'), {}, a:options, 1)
	elseif l:places ==# 'w'
	    if &l:buftype ==# 'nofile' && (bufname('') ==# (v:version < 702 ? 'command-line' : '[Command Line]') || bufname('') ==# 'option-window')
		" In the command-line window, we cannot temporarily leave it to
		" search in other windows: "E11: Invalid in command-line
		" window". Work around this by performing the buffer search for
		" those visible buffers. (Unless a custom extractor is used.)
		if ! has_key(a:options, 'extractor')
		    call s:FindInOtherBuffers(l:searchedBuffers, a:matches, function('s:MatchInBuffer'), a:options, tabpagebuflist())
		endif
	    else
		call s:FindInOtherWindows(l:searchedBuffers, a:matches, function('s:MatchInCurrent'), a:options)
	    endif
	elseif l:places ==# 'b'
	    call s:FindInOtherBuffers(l:searchedBuffers, a:matches, function('s:MatchInBuffer'), a:options, s:GetListedBufnrs())
	endif
    endfor
    unlet s:patterns
endfunction
function! CompleteHelper#Find( matches, Funcref, options )
"*******************************************************************************
"* PURPOSE:
"   Find matches by invoking a:Funcref with a:options and store them in
"   a:matches.
"* ASSUMPTIONS / PRECONDITIONS:
"   none
"* EFFECTS / POSTCONDITIONS:
"   none
"* INPUTS:
"   a:matches	(Empty) List that will hold the matches (in Dictionary format,
"		cp. :help complete-functions). Matches will be appended.
"   a:Funcref   Funcref that is invoked in each buffer to extract the completion
"		matches. Is passed the following arguments:
"		a:lines     List of lines to be searched in a buffer that
"			    currently isn't visible in a window, or is unloaded.
"			    If empty List, the current window should be searched
"			    instead.
"		a:matches   Passed through List that will hold the matches.
"		a:matchTemplate Template object for an added match. Clone and
"				add attributes.
"		a:options   Passed through Dictionary with Funcref
"			    configuration.
"		a:isInCompletionWindow  Flag whether this is the window where
"					the completion was originally triggered
"					by the user (to be able to exclude
"					matches at the cursor position).
"   a:options	Dictionary with Funcref configuration:
"   a:options.complete	    Specifies what is searched, like the 'complete'
"			    option. Supported options: '.' for current buffer,
"			    'w' for buffers from other windows, 'b' for other
"			    loaded buffers from the buffer list.
"   a:options.bufferPredicate   Funcref that decides whether a particular buffer
"				should be searched. It is passed a buffer number
"				and must return 0 when the buffer should be
"				skipped.
"		Other options can be added to be consumed by the Funcref, which
"		gets these all passed.
"* RETURN VALUES:
"   a:matches
"*******************************************************************************
    let l:complete = get(a:options, 'complete', '')
    let l:searchedBuffers = {}
    for l:places in split(l:complete, ',')
	if l:places ==# '.'
	    call s:FindInCurrentWindow(l:searchedBuffers, a:matches, a:Funcref, {}, a:options, 1)
	elseif l:places ==# 'w'
	    if &l:buftype ==# 'nofile' && (bufname('') ==# (v:version < 702 ? 'command-line' : '[Command Line]') || bufname('') ==# 'option-window')
		" In the command-line window, we cannot temporarily leave it to
		" search in other windows: "E11: Invalid in command-line
		" window". Work around this by performing the buffer search for
		" those visible buffers. (Unless a custom extractor is used.)
		if ! has_key(a:options, 'extractor')
		    call s:FindInOtherBuffers(l:searchedBuffers, a:matches, a:Funcref, a:options, tabpagebuflist())
		endif
	    else
		call s:FindInOtherWindows(l:searchedBuffers, a:matches, a:Funcref, a:options)
	    endif
	elseif l:places ==# 'b'
	    call s:FindInOtherBuffers(l:searchedBuffers, a:matches, a:Funcref, a:options, s:GetListedBufnrs())
	endif
    endfor
endfunction

" Deprecated. Use CompleteHelper#Abbreviate#Word() instead.
function! CompleteHelper#Abbreviate( matchObj )
    return CompleteHelper#Abbreviate#Word(a:matchObj)
endfunction

function! CompleteHelper#JoinMultiline( text )
"******************************************************************************
"* PURPOSE:
"   Replace newline(s) plus any surrounding whitespace with a single <Space>.
"   Insert mode completion currently does not deal sensibly with multi-line
"   completions (newlines are inserted literally as ^@), so completions may want
"   to do processing to offer a better behavior.
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"* EFFECTS / POSTCONDITIONS:
"   None.
"* INPUTS:
"   a:text
"* RETURN VALUES:
"   Contents of a:text joined into a single line without newline characters.
"******************************************************************************
    return (stridx(a:text, "\n") == -1 ? a:text : substitute(a:text, "\\%(\\s*\n\\)\\+\\s*", ' ', 'g'))
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
