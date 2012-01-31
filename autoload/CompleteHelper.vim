" CompleteHelper.vim: Generic functions to support custom insert mode completions. 
"
" DEPENDENCIES:
"   - EchoWithoutScrolling.vim autoload script for CompleteHelper#Abbreviate(). 
"
" Copyright: (C) 2008-2012 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'. 
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS 
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

function! CompleteHelper#ExtractText( startPos, endPos, matchObj )
"*******************************************************************************
"* PURPOSE:
"   Extract the text between a:startPos and a:endPos from the current buffer. 
"   Multiple lines will be delimited by a newline character. 
"* ASSUMPTIONS / PRECONDITIONS:
"   none
"* EFFECTS / POSTCONDITIONS:
"   none
"* INPUTS:
"   a:startPos	    [line,col]
"   a:endPos	    [line,col]
"   a:matchObj	    The match object to be returned to the completion function. 
"		    This function does not need to set anything there, the
"		    mandatory matchObj.word will be set from this function's
"		    return value automatically (and with additional processing).
"		    However, you _can_ modify other items if you deem necessary.
"		    (E.g. add a note to matchObj.menu that the text was
"		    truncated.) 
"* RETURN VALUES: 
"   string text; return an empty string to signal that no match should be added
"   to the list of matches. 
"*******************************************************************************
    let [l:line, l:column] = a:startPos
    let [l:endLine, l:endColumn] = a:endPos
    if l:line > l:endLine || (l:line == l:endLine && l:column > l:endColumn)
	return ''
    endif

    let l:text = ''
    while 1
	if l:line == l:endLine
	    let l:text .= matchstr( getline(l:line) . "\n", '\%' . l:column . 'c' . '.*\%' . (l:endColumn + 1) . 'c' )
	    break
	else
	    let l:text .= matchstr( getline(l:line) . "\n", '\%' . l:column . 'c' . '.*' )
	    let l:line += 1
	    let l:column = 1
	endif
    endwhile
    return l:text
endfunction
function! s:FindMatchesInCurrentWindow( matches, pattern, matchTemplate, options, isInCompletionBuffer )
    let l:isBackward = has_key(a:options, 'backward_search')

    let l:save_cursor = getpos('.')

    let l:firstMatchPos = [0,0]
    while ! complete_check()
	let l:matchPos = searchpos( a:pattern, 'w' . (l:isBackward ? 'b' : '') )
	if l:matchPos == [0,0] || l:matchPos == l:firstMatchPos
	    " Stop when no matches or wrapped around to first match. 
	    break
	endif
	if l:firstMatchPos == [0,0]
	    " Record first match position to detect wrap-around. 
	    let l:firstMatchPos = l:matchPos
	endif

	let l:matchEndPos = searchpos( a:pattern, 'cen' )
	if a:isInCompletionBuffer && (l:matchEndPos == l:save_cursor[1:2])
	    " Do not include a match ending at the cursor position; this is just
	    " the completion base, and Vim would not offer this anyway. Such a
	    " match can happen if a:base =~ a:pattern. 
	    continue
	endif

	" Initialize the match object and extract the match text. 
	let l:matchObj = copy(a:matchTemplate)
	let l:matchText = (has_key(a:options, 'extractor') ? a:options.extractor(l:matchPos, l:matchEndPos, l:matchObj) : CompleteHelper#ExtractText(l:matchPos, l:matchEndPos, l:matchObj))

	" Custom processing of match text. 
	if has_key(a:options, 'processor')
	    let l:matchText = a:options.processor(l:matchText)
	endif

	" Store match text in match object. 
	let l:matchObj.word = l:matchText

	" Only add if this is an actual match that is not yet in the list of
	" matches. 
	if ! empty(l:matchText) && index(a:matches, l:matchObj) == -1
	    call add( a:matches, l:matchObj )
	endif
"****D echomsg '**** match from' string(l:matchPos) 'to' string(l:matchEndPos) l:matchText
    endwhile
    
    call setpos('.', l:save_cursor)
endfunction
function! s:FindMatchesInOtherWindows( matches, pattern, options )
    let l:searchedBuffers = { bufnr('') : 1 }
    let l:originalWinNr = winnr()

    " By entering a window, its height is potentially increased from 0 to 1 (the
    " minimum for the current window). To avoid any modification, save the window
    " sizes and restore them after visiting all windows. 
    let l:originalWindowLayout = winrestcmd()

    for l:winNr in range(1, winnr('$'))
	execute 'noautocmd' l:winNr . 'wincmd w'

	let l:matchTemplate = { 'menu': bufname('') }

	if ! has_key( l:searchedBuffers, bufnr('') )
	    call s:FindMatchesInCurrentWindow( a:matches, a:pattern, l:matchTemplate, a:options, 0 )
	    let l:searchedBuffers[ bufnr('') ] = 1
	endif
    endfor

    execute 'noautocmd' l:originalWinNr . 'wincmd w'
    silent! execute l:originalWindowLayout
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
"   a:options	Dictionary with match configuration:
"   a:options.complete	    Specifies what is searched, like the 'complete'
"			    option. Supported options: '.' for current buffer, 
"			    'w' for buffers from other windows. 
"   a:options.backward_search	Flag whether to search backwards from the cursor
"				position. 
"   a:options.extractor	    Function reference that extracts the matched text
"			    from the current buffer. Will be invoked with
"			    ([startLine, startCol], [endLine, endCol], matchObj)
"			    arguments with the cursor positioned at the start of
"			    the current match; must return string; can modify
"			    the initial matchObj. 
"   a:options.processor	    Function reference that processes matches. Will be
"			    invoked with an a:matchText argument; must return
"			    processed string, or empty string if the match
"			    should be discarded. Alternatively, you can filter()
"			    / map() the a:matches result returned from this
"			    function, but passing in a function may be easier
"			    for you. 
"* RETURN VALUES: 
"   a:matches
"*******************************************************************************
    let l:complete = get(a:options, 'complete', '')
    for l:places in split(l:complete, ',')
	if l:places == '.'
	    call s:FindMatchesInCurrentWindow( a:matches, a:pattern, {}, a:options, 1 )
	elseif l:places == 'w'
	    call s:FindMatchesInOtherWindows( a:matches, a:pattern, a:options )
	endif
    endfor
endfunction

function! s:ListChar( settingFilter, fallback )
    let listchar = matchstr(&listchars, a:settingFilter)
    return (empty(listchar) ? a:fallback : listchar)
endfunction
let s:tabReplacement = s:ListChar('tab:\zs..', '^I')
let s:eolReplacement = s:ListChar('eol:\zs.', '^J') " Vim commands like :reg show newlines as ^J.
delfunction s:ListChar

function! CompleteHelper#Abbreviate( matchObj )
"******************************************************************************
"* PURPOSE:
"   Shorten the match abbreviation and change (invisible) <Tab> and newline
"   characters to what's defined in 'listchars'. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   None. 
"* INPUTS:
"   a:matchObj	    The match object to be returned to the completion function. 
"* RETURN VALUES: 
"   Extended match object with 'abbr' attribute. 
"******************************************************************************
    let l:abbreviatedMatch = substitute(a:matchObj.word, '\t', s:tabReplacement, 'g')
    let l:abbreviatedMatch = substitute(l:abbreviatedMatch, "\n", s:eolReplacement, 'g')

    let l:maxDisplayLen = &columns / 2
    if len(l:abbreviatedMatch) > l:maxDisplayLen
	let a:matchObj.abbr = EchoWithoutScrolling#TruncateTo( l:abbreviatedMatch, l:maxDisplayLen )
    elseif l:abbreviatedMatch !=# a:matchObj.word
	let a:matchObj.abbr = l:abbreviatedMatch
    endif

    return a:matchObj
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

" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
