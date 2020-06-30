/** Overwrite default failsafe gem_confirm
*/
function gem_confirm(message, callback)
{
    if ( $("#modal-dialog-confirm").length < 1 )
    {
        $('<div id="modal-dialog-confirm"></div>').appendTo('body');
        $("#modal-dialog-confirm").dialog({
            autoOpen: false,
            modal: true,
            position: ['center','middle'],
            buttons: {
                "Yes": function() {
                    $(this).dialog("close");
                    callback();
                    },
                "No": function() {
                    $(this).dialog("close");
                    }
                }
            });
    }
    $("#modal-dialog-confirm").html('<p><span class="ui-icon ui-icon-alert" style="float:left; margin:0 7px 20px 0;"></span>'+message+'</p>');
    $("#modal-dialog-confirm").dialog("open");
}

/** Overwrite default failsafe gem_alert
*/
function gem_alert(message)
{
    if ( $("#modal-dialog-alert").length < 1 )
    {
        $('<div id="modal-dialog-alert"></div>').appendTo('body');
        $("#modal-dialog-alert").dialog({
            autoOpen: false,
            modal: true,
            position: ['center','middle'],
            buttons: {
                "Ok": function() {
                    $(this).dialog("close");
                    }
                }
            });
    }
    $("#modal-dialog-alert").html(message);
    $("#modal-dialog-alert").dialog("open");
}

$(document).ready(function()
{

    // Setup ajax
    $.ajaxSetup({ timeout: 300000, cache: false });
    // Let our server know we have JS on client side
    $("input[name='jsenabled']").val("1");
    // Show JS related xhtml nodes
    $(".jsonly").show();

});
