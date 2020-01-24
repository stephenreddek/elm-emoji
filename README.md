# Elmoji

This is a fork of [teepark/elmoji](https://github.com/teepark/elmoji) because it was not upgraded to 0.19.

## Seamlessly display emoji in Elm applications

This library can comb through a string, identify the emojis within it, and
produce html with all emoji characters replaced with free CDN-hosted images.

## Next steps
Consider if we should parse out possible emojis
and then lookup to see if they're in the giant dictionary
This would mean no false-positives but perhaps false-negatives
The benefit is that the code may be more readable and
    perhaps the performance is better since the comparisons are simpler
    Perhaps its also better because we aren't later dropping text off the left etc. Iterating once.

\u{ufe0e} means show as text not as an emoji

## References
* https://thekevinscott.com/emojis-in-javascript/
* https://github.com/thekevinscott/emoji-tree/blob/master/lib/emojiRegex.js
* https://github.com/thekevinscott/emoji-tree/issues/2
* https://github.com/beaugunderson/emoji-aware/issues/11
* https://en.wikipedia.org/wiki/Emoji#Unicode_blocks
* http://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules
* http://www.unicode.org/emoji/charts/full-emoji-list.html
* https://unicode.org/Public/emoji/12.0/emoji-test.txt
* https://github.com/emojione/emojione/blob/master/lib/python/emojipy/emojipy.py
* https://package.elm-lang.org/packages/elm/parser/latest/Parser
* https://github.com/elm/parser/blob/master/semantics.md
* https://github.com/beaugunderson/emoji-aware
* https://mathiasbynens.be/notes/javascript-encoding#surrogate-formulae
* https://github.com/emojione/emojione/issues/191