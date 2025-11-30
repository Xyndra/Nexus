; Bracket pairs for matching

("[" @open "]" @close)
("{" @open "}" @close)
("(" @open ")" @close)
("<" @open ">" @close)

; String quotes - exclude from rainbow brackets
(("\"" @open "\"" @close) (#set! rainbow.exclude))
(("'" @open "'" @close) (#set! rainbow.exclude))
