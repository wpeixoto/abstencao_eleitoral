
/* Merged Plone Javascript file
 * This file is dynamically assembled from separate parts.
 * Some of these parts have 3rd party licenses or copyright information attached
 * Such information is valid for that section,
 * not for the entire composite file
 * originating files are separated by - filename.js -
 */

/* - searchJE.js - */
// http://www.tse.jus.br/portal_javascripts/searchJE.js?original=1
jQuery(function($){var query,pushState,popped,initialURL,$default_res_container=$('#search-results'),$search_filter=$('#search-filter'),$search_field=$('#search-field'),$search_gadget=$('#searchGadget'),$form_search_page=$("form.searchPage"),navigation_root_url=$('meta[name=navigation_root_url]').attr('content')||window.navigation_root_url||window.portal_url;$.fn.pullSearchResults=function(query){return this.each(function(){var $container=$(this);$.get('@@updated_search',query,
function(data){$container.hide();var $ajax_search_res=$('<div id="ajax-search-res"></div>').html(data),$search_term=$('#search-term');var $data_res=$ajax_search_res.find('#search-results').children(),data_search_term=$ajax_search_res.find('#updated-search-term').text(),data_res_number=$ajax_search_res.find('#updated-search-results-number').text(),data_sorting_opt=$ajax_search_res.find('#updated-sorting-options').html();$container.html($data_res);$container.fadeIn();if(!$search_term.length){$search_term=$('<strong id="search-term" />').appendTo('h1.documentFirstHeading')}
$search_term.text(data_search_term);$('#search-results-number').text(data_res_number);$('#search-results-bar').find('#sorting-options').html(data_sorting_opt);$('#rss-subscriptionJE').find('a.link-feed').attr('href', function(){return navigation_root_url+'/searchRSS?'+query})})})};pushState=function(query){if(Modernizr.history){var url=navigation_root_url+'/@@search?'+query;history.pushState(null,null,url)}};popped=(window.history&&'state' in window.history);initialURL=location.href;$(window).bind('popstate', function(event){var initialPop,str;initialPop=!popped&&location.href===initialURL;popped=true;if(initialPop){return}
if(!location.search){return}
query=location.search.split('?')[1];var results=query.match(/SearchableText=[^&]*/);if(results){str=results[0];str=decodeURIComponent(str.replace(/\+/g, ' ')); // we remove '+' used between words
$.merge($search_field.find('input[name="SearchableText"]'),$search_gadget).val(str.substr(15,str.length));$default_res_container.pullSearchResults(query)}});$search_filter.find('input.searchPage[type="submit"]').hide();$search_field.find('input.searchButton').click(function(e){var st,queryString=location.search.substring(1),re=/([^&=]+)=([^&]*)/g,m,queryParameters=[],key;st=$search_field.find('input[name="SearchableText"]').val();queryParameters.push({"name":"SearchableText","value":st});while(m=re.exec(queryString)){key=decodeURIComponent(m[1]);if(key!=='SearchableText'){queryParameters.push({"name":key,"value":decodeURIComponent(m[2].replace(/\+/g,' '))})}}
queryString=$.param(queryParameters);$default_res_container.pullSearchResults(queryString);pushState(queryString);e.preventDefault()});$form_search_page.submit(function(e){query=$(this).serialize();$default_res_container.pullSearchResults(query);pushState(query);e.preventDefault()});$search_field.find('input[name="SearchableText"]').keyup(function(){$search_gadget.val($(this).val())});$('#search-results-bar').find('dl.actionMenu > dd.actionMenuContent').click(function(e){e.stopImmediatePropagation()});$search_filter.delegate('input, select','change',
function(e){query='';if($search_filter.find('input:checked').length>1){query=$form_search_page.serialize()}
$default_res_container.pullSearchResults(query);pushState(query);e.preventDefault()});$('#sorting-options').delegate('a','click', function(e){if($(this).attr('data-sort')){$form_search_page.find("input[name='sort_on']").val($(this).attr('data-sort'))}
else{$form_search_page.find("input[name='sort_on']").val('')}
query=this.search.split('?')[1];$default_res_container.pullSearchResults(query);pushState(query);e.preventDefault()});$default_res_container.delegate('.listingBar a','click', function(e){query=this.search.split('?')[1];$default_res_container.pullSearchResults(query);pushState(query);e.preventDefault()})});

/* - ++resource++plone.formwidget.masterselect/masterselect.js - */
// http://www.tse.jus.br/portal_javascripts/++resource++plone.formwidget.masterselect/masterselect.js?original=1
(function($){var cache={};var masterVocabularyQueue={};var masterVocabularyComplete={};
function sprintf(format,etc){var arg=arguments;var i=1;return format.replace(/%((%)|s)/g, function(m){return m[2]||arg[i++]})}
function populateSelectOptions(items){var options='';var selected='';for(var i=0;i<items.length;i++){selected=items[i].selected?' selected="selected"':'';options+=sprintf('<option id="%s" value="%s"%s>%s</option>',items[i].id,items[i].value,selected,items[i].content)}
return options}
function updateSelect(slaveID,data){$(slaveID).closest('select').empty().html(populateSelectOptions(data.items)).end().change().attr('disabled',false)};
function handleMasterVocabularyChange(event){var value=$(this).attr('type')=='checkbox'?''+this.checked:$(this).val();var slaveID=event.data.slaveID;var name=event.data.name;var masterID=event.data.masterID;var cachekey=[this.id,slaveID,value].join(':');$(slaveID).find('option').slice(event.data.empty_length).remove();$(slaveID).change();$(slaveID).closest(':input').attr('disabled',true);if(masterVocabularyQueue[event.data.slaveID]){masterVocabularyQueue[event.data.slaveID].abort();delete(masterVocabularyQueue[event.data.slaveID])}
var prevent_ajax_values=event.data.prevent_ajax_values!=undefined?event.data.prevent_ajax_values:[]
if(typeof prevent_ajax_values=='string')
prevent_ajax_values=[prevent_ajax_values];var val=$(this).attr('type')=='checkbox'?this.checked:$(this).val();val=prevent_ajax_values.length==0?false:$.inArray(val,prevent_ajax_values)>-1;if(val)
return;var queuekey=event.data.masterID;if(masterVocabularyQueue[queuekey]){masterVocabularyQueue[queuekey].abort();delete(masterVocabularyQueue[queuekey])}
if(value!=null){if(cache[cachekey]==undefined){masterVocabularyQueue[queuekey]=$.getJSON(event.data.url,{field:this.id,name:name,slaveID:slaveID,masterID:masterID,value:value},
function(data){cache[cachekey]=data;updateSelect(slaveID,data)})}
else
updateSelect(slaveID,cache[cachekey])}};$.fn.bindMasterSlaveVocabulary=function(data){var trigger=data.initial_trigger?data.initial_trigger:false;var emptyLength=data.empty_length?data.empty_length:0
var slaveLength=$(data.slaveID)[0].length;if(slaveLength<=emptyLength)
$(data.slaveID).attr('disabled',true);$(this).on('change',data,handleMasterVocabularyChange);if(trigger)
$(this).trigger('change')};
function updateAttr(slaveID,data){$(slaveID).attr(data.attr,data.value).change()}
function handleMasterAttrChange(event){var value=$(this).attr('type')=='checkbox'?''+this.checked:$(this).val();var slaveID=event.data.slaveID;var name=event.data.name;var masterID=event.data.masterID;var cachekey=[this.id,slaveID,value].join(':');if(cache[cachekey]==undefined)
$.getJSON(event.data.url,{field:this.id,slaveID:slaveID,name:name,masterID:masterID,value:value},
function(data){cache[cachekey]=data;updateAttr(slaveID,data)});else updateAttr(slaveID,cache[cachekey])};$.fn.bindMasterSlaveAttr=function(data){var trigger=data.initial_trigger?data.initial_trigger:true;$(this).on('change',data,handleMasterAttrChange);if(trigger)
$(this).trigger('change')};
function updateValue(slaveID,data){var slaveID=event.data.form.find(event.data.slaveID);slaveID.val(data).change();if(slaveID.is('.kupu-editor-textarea'))
slaveID.siblings('iframe:first').contents().find('body').html(data)}
function handleMasterValueChange(event){var value=$(this).attr('type')=='checkbox'?''+this.checked:$(this).val();var slaveID=event.data.slaveID;var name=event.data.name;var masterID=event.data.masterID;var cachekey=[this.id,slaveID,value].join(':');if(cache[cachekey]==undefined)
$.getJSON(event.data.url,{field:this.id,slaveID:slaveID,name:name,masterID:masterID,value:value},
function(data){cache[cachekey]=data;updateValue(slaveID,data)});else updateValue(slaveID,cache[cachekey])};$.fn.bindMasterSlaveValue=function(data){var trigger=data.initial_trigger?data.initial_trigger:true;data.form=$(this).parents('form').first();$(this).on('change',data,handleMasterValueChange);if(trigger)
$(this).trigger('change')};
function handleMasterToggle(event){var action=event.data.action;var slaveID=event.data.form.find(event.data.slaveID);var val=$(this).attr('type')=='checkbox'?this.checked:$(this).val();val=event.data.values.length==0?true:$.inArray(val,event.data.values)>-1;if($.inArray(action,['hide','disable'])>-1){val=!val;action=action=='hide'?'show':'enable'}
if(action=='show'){var selector=event.data.siblings?slaveID.parent():slaveID;var css_action=val?"show":"hide";var css_option=event.data.initial_trigger?null:'fast';selector.each(function(){$(this)[css_action](css_option)})} else
slaveID.closest(':input').attr('disabled',val?false:true)}
$.fn.bindMasterSlaveToggle=function(data){var master=$(this);data.form=master.parents('form').first();var trigger=data.initial_trigger?data.initial_trigger:true;data.initial_trigger=trigger;master.on('change',data,handleMasterToggle);if(data.initial_trigger){var fieldset_id=master.closest('fieldset').attr('id');if(fieldset_id===undefined||$(fieldset_id).is(":visible")){master.change()} else{fieldset_id='#'+fieldset_id;var props={position:'absolute',visibility:'hidden',display:'block'};var old_props={};for(var name in props){old_props[name]=$(fieldset_id).css(name)}
$(fieldset_id).css(props);master.change();$(fieldset_id).css(old_props)}
data.initial_trigger=false}}})(jQuery);
