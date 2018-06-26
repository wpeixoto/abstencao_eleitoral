# A abstenção eleitoral tem crescido?

Um jornal [afirma](http://www.tse.jus.br/imprensa/noticias-tse/2014/Maio/tse-abstencao-dos-eleitores-causa-prejuizo-ao-contribuinte) que a abstenção eleitoral vem crescendo desde 1998, apresentando apenas as médias. Este estudo visa utilizar os conhecimentos adquiridos na disciplina para verificar ou refutar essa informação, mesmo sabendo que isso pode não ser possível com os dados disponíveis.

[Outro artigo](https://politica.estadao.com.br/noticias/geral,envelhecimento-do-eleitorado-explica-14-da-abstencao,70002204016) atribui 1/4 da abstenção à falta de obrigatoriedade do comparecimento às urnas depois de 60 anos, mas não oferece dados. Os que consegui no TSE não individualizam as ausências, e portanto não consigo verificar diretamente se os mais idosos realmente faltam mais.

## Hipóteses

O jornal afirma que a abstenção está crescendo. Mas realemnte está?

A hipótese a ser testada (H<sub>a</sub>) é: A abstenção eleitoral aumenta de uma eleição para outra. Portanto, a hipótese nula (H<sub>0</sub>) será a de que não há diferença relevante entre as taxas de abstenção de um ano para outro.

### Pareado ou independente?
A população é todo o eleitorado do DF. Contudo, essa população varia de um ano eleitoral para outro. Espera-se que grande parte dos eleitores de uma eleição continue o sendo nas eleições seguintes. As variações possíveis são:
- Novos eleitores a partir de 16 anos (opcionalmente) e 18 anos (obrigados a votar)
- Mudanças de domicílio eleitoral (para fora e para dentro)
- Idosos desobrigados de votar.

A possibilidade de abstenção é objeto deste estudo, e portanto não será considerada parte de mudança de população.

O segundo turno, por ocorrer imediatamente após o primeiro, e por isso poderia ser considerado um experimento *pareado*, com a mesma população em momentos distintos. Contudo, a quantidade de eleitores aptos a votar não parece ser igual: Em 2002, foram contados 1.516.598 aptos no primeiro turno e 1.518.437 no segundo, 1.839 a nais (mas ainda não verifiquei se isso se deve a erros nos dados)

Por um lado, não sei estimar o impacto desses eventos na suposição de que a população seja a mesma. Por outro, ao forçar a aplicação do [Paradoxo do Navio de Teseu](https://pt.wikipedia.org/wiki/Navio_de_Teseu), posso considerar que a população em duas eleições seucessivas seja uma sucessora parecida o suficiente. Portanto, parece razoável que a população seja considerada a mesma pelo menos para eleições sucessivas.

Por outro, considerar as amostras independentes pode ser uma decisão mais conservadora, mais rigorosa.


## Andamentos

- Descobri sujeira nos dados. Melhor verificar tudo de novo 


## Outras pesquisas
Por enquanto, encontrei apenas essa:
- [CULTURA POLÍTICA E ABSTENÇÃO ELEITORAL](http://e-legis.camara.leg.br/cefor/index.php/e-legis/article/view/279/369), de Hemerson Luiz Pase, Luis Gustavo Teixeira Silva, Everton Rodrigo Santos, publicada na revista e-Legis V.9, n.21, Set/Dez.2016
