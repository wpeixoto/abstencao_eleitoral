jq(function () {
    jq("#formulario-feed").validate({
        // Specify validation rules
        rules: {
            feed_email: {
                required: true,
                email: true
            },
            feed_experiencia_site: {
                required: true
            },
            feed_sugestao: {
                require_from_group: [1,'.textarea-feed']
            },
            feed_elogio: {
                require_from_group: [1,'.textarea-feed']
            },
            feed_criticas: {
                require_from_group: [1,'.textarea-feed']
            },
            feed_erro: {
                require_from_group: [1,'.textarea-feed']
            },
            feed_nao_encontrou: {
                require_from_group: [1,'.textarea-feed']
            },
        },
        // Specify validation error messages
        messages: {
            feed_email: {
                required: "Por favor, informe o email.",
                email: "Por favor, informe um email válido."
            },
            feed_experiencia_site: {
                required: "Por favor, informe como foi sua experiência."
            },
            feed_sugestao: {
                require_from_group: "Por favor, inclua ao menos 1(um) feedback"
            }
        },
        errorPlacement: function(error, element) {
            error.insertAfter("#Sugestaolabel");
        },
        // Make sure the form is submitted to the destination defined
        // in the "action" attribute of the form when valid
        submitHandler: function (form) {
            //form.submit();
        }
    });
});
jq("#formulario-feed").submit(function(e){
    if (jq("#formulario-feed").valid()){
        var captcha = grecaptcha.getResponse(widgetFeedback);
        if(captcha == "") {
            jq('#captcha-feedback').addClass("requiredCaptcha");
            e.preventDefault();
        } else {
            jq('#captcha-feedback').removeClass();
            var postData = jq(this).serializeArray();
            var formURL = jq(this).attr("action");
            //retirar o formulário
            jq('#form').fadeOut('fast');
            //mostra o GIF animado
            jq('.loading-send-feed').fadeIn('fast');
            jq('#ajax-spinner').fadeOut('fast');
            jq('#captcha-feedback').hide();
            jq('#btn-enviar').hide();
            jq('#btn-cancel').hide();
            $.ajax({
                url : formURL,
                type: "POST",
                timeout: 5000,
                data : postData,
                success:function(data, textStatus, jqXHR)
                {
                    //tira o GIF animado da tela apos a resposta do servico
                    jq('.loading-send-feed').fadeOut('fast');
                    jq('#ajax-spinner').fadeOut('fast');
                    jq('#form').fadeOut('fast');
                    jq('#captcha-feedback').hide();
                    jq('#btn-enviar').hide();
                    jq('#btn-cancel').hide();
                    jq('#resp').html('Mensagem enviada com sucesso. <br />Agradecemos sua participação!');
                    jq('#resp').fadeIn('fast');
                    jq('#btn-close').show();
                    jq("a[id^='ui-tab-").removeClass('marcar-texto-feed')
                    jq(".radio-inline").removeClass('active')
                    document.getElementById("formulario-feed").reset();
                    grecaptcha.reset(widgetFeedback);

                },
                error: function(jqXHR, textStatus, errorThrown)
                {
                    //tira o GIF animado da tela apos a resposta do servico
                    jq('.loading-send-feed').fadeOut('fast');
                    jq('#ajax-spinner').fadeOut('fast');
                    jq('#form').fadeOut('fast');
                    jq('#captcha-feedback').hide();
                    jq('#btn-enviar').hide();
                    jq('#btn-cancel').hide();
                    jq('#resp').html('Ocorreu um erro e sua mensagem não foi enviada.<br />Por favor, entre em contato pelo e-mail <a href="mailto:8800@tse.jus.br">8800@tse.jus.br</a>.');
                    jq('#resp').fadeIn('fast');
                    jq('#btn-close').show();
                    jq("a[id^='ui-tab-").removeClass('marcar-texto-feed')
                    jq(".radio-inline").removeClass('active')
                    document.getElementById("formulario-feed").reset();
                    grecaptcha.reset(widgetFeedback);
                }
            });
            e.preventDefault();
        }
    }
});
function resetFeed() {
    jq("#resp").html('');
    jq('#resp').fadeOut('fast');
    jq('#form').fadeIn('fast');
    jq('#captcha-feedback').show();
    jq('#btn-enviar').show();
    jq('#btn-cancel').show();
    jq('#btn-close').hide();
    jq("a[id^='ui-tab-").removeClass('marcar-texto-feed')
    jq(".radio-inline").removeClass('active')
    document.getElementById("formulario-feed").reset();
}