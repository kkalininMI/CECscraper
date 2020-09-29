$(document).ready(function(){

    $.datepicker.setDefaults($.datepicker.regional['ru']);
    $( "#start_date" ).datepicker({
      changeMonth: true,
	  changeYear: true,
      onClose: function( selectedDate ) {
        $( "#end_date" ).datepicker( "option", "minDate", selectedDate );
      }
    });
    $( "#end_date" ).datepicker({
      changeMonth: true,
	  changeYear: true,
      onClose: function( selectedDate ) {
        $( "#start_date" ).datepicker( "option", "maxDate", selectedDate );
      }
    });

});

function initDate4Href(start_date, end_date){
    document.forms["calendar"].start_date.value = start_date;
    document.forms["calendar"].end_date.value = end_date;
	document.forms["calendar"].submit();
}