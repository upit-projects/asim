/*
Copyright (c) 2003-2010, CKSource - Frederico Knabben. All rights reserved.
For licensing, see LICENSE.html or http://ckeditor.com/license
*/

CKEDITOR.editorConfig = function( config )
{
    config.entities_greek = false;
	config.entities_latin = false;
};
CKEDITOR.on('instanceReady', function(ev)
{

        ev.editor.dataProcessor.writer.lineBreakChars = '\n\n';

        // Do not break
        var tags = ['span', 'a', 'b', 'strong', 'img', 'font'];
        for (var key in tags) {
        ev.editor.dataProcessor.writer.setRules(tags[key],
        {
            indent : false,
            breakBeforeOpen : false,
            breakAfterOpen : false,
            breakBeforeClose : false,
            breakAfterClose : false
        });
        }

        // breakBeforeOpen, breakAfterClose
        var tags = ['p', 'ol', 'ul', 'div', 'table', 'thead', 'tbody', 'tr', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6'];
        for (var key in tags) {
        ev.editor.dataProcessor.writer.setRules(tags[key],
        {
            indent : false,
            breakBeforeOpen : true,
            breakAfterOpen : false,
            breakBeforeClose : false,
            breakAfterClose : true
        });
        }

        // breakBeforeOpen
        var tags = ['li', 'td', 'th'];
        for (var key in tags) {
        ev.editor.dataProcessor.writer.setRules(tags[key],
        {
            indent : false,
            breakBeforeOpen : true,
            breakAfterOpen : false,
            breakBeforeClose : false,
            breakAfterClose : false
        });
        }

});
