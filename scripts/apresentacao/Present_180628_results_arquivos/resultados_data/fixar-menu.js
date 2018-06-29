// JavaScript Document


//script do menu das unidades do TSE
function menu_repositorio()
{
	$("#"+$(".bread_menu").attr("title")).addClass("selecionado-ml");
	$("#"+$("#controle_anos").attr("title")).addClass("selecionado-ms");
}


$(document).ready(function() {
  menu_repositorio();
});
