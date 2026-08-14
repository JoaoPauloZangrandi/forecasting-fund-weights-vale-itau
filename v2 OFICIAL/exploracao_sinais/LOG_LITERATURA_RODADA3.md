# Pesquisa de literatura — rodada 3 (14/08/2026, 5 ângulos novos)

## Achados novos genuinamente testáveis

1. **CIO Peer Momentum (Ying 2024, JFE)** — ações com donos institucionais
   em comum têm cross-predictability em retorno MENSAL (frequência que bate
   com nosso painel — diferente do Comomentum, que precisava de dado
   semanal e por isso deu artefato). Mecanismo: difusão gradual de
   informação através de rede de investidores institucionais.
   **⚠️ Ressalva crítica (Burt & Hrdlicka 2021, JFQA):** decompõe esse tipo
   de previsibilidade (família "Economic Links") em (a) contágio genuíno
   de informação e (b) "commonality in momentum" — comovimento MECÂNICO
   sem nada de informacional. Em horizonte de 1 mês as duas fontes
   contribuem quase igual; em horizontes mais longos é **inteiramente**
   artefato mecânico. Implementando com cuidado extra por causa disso —
   se achar algo, preciso checar se sobrevive controlando pelo momentum
   da própria ação antes de acreditar.

2. **Institutional ownership × momentum (interação condicional, China
   A-shares 2023)** — em ações com alta propriedade institucional, o
   momentum é mais forte (menos ruído de retail); em baixa propriedade,
   mais fraco. Mais simples de implementar (interação com momentum que já
   temos calculado). Evidência de mercado emergente, sem réplica brasileira
   confirmada.

## Ângulos que voltaram vazios (honestamente reportado pela pesquisa)

- **ML/não-linear em sinais de holdings**: não existe literatura
  re-testando sinais tipo Koijen-Yogo/FIT/breadth com random forest/GBM —
  gap genuíno, não candidato pronto.
- **Decaimento intra-mês**: sem sustentação de literatura (o paper mais
  próximo usa dado de transação de 12 meses, não dias, que não temos).
- **Brasil não-Koijen**: literatura brasileira de "smart money" é a nível
  de FUNDO (fluxo persegue performance do fundo), não a nível de AÇÃO
  prevendo retorno — nível de agregação errado pro que precisamos. E
  mesmo essa literatura é inconsistente entre subperíodos.
