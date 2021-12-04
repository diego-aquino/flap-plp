# Guia de contribuição

Este é um documento colaborativo entre os participantes do projeto. Sinta-se
livre para fazer sugestões caso tenha ideias ou gostaria de mudar alguma
convenção.

## Instalação

```bash
git clone https://github.com/diego-aquino/flap-plp.git
cd flap-plp
```

## Convenções

### Branches

Este projeto conta com duas branches centrais:

- `main` como a branch principal, contendo a versão oficial do projeto e pronta
  para ser utilizada pelo público.
- `canary` como a branch secundária, contendo a versão testada e mais atualizada
  do projeto.

Durante o desenvolvimento, o padrão para criar branches é o seguinte:

- `feature/<branch-subject>`: branches para o desenvolvimento de uma nova
  funcionalidade
- `fix/<branch-subject>`: branches para correção de um problema ou bug
- `docs/<branch-subject>`: branches para mudanças na documentação do projeto
- `refactor/<branch-subject>`: branches para refatorações que não adicionam
  funcionalidades nem corrigem bugs
- `style/<branch-subject>`: branches para mudanças em estilos e lints

> Exemplos: `feature/game-results-screen`, `fix/game-progress-not-being-saved` e
> `docs/readme-contributors-section`

### Mensagens de commit

Este projeto segue o padrão [conventional
commits](https://www.conventionalcommits.org), como forma de padronizar as
mensagens de commit. Por isso, cada commit deve estar em inglês (?) e seguir a
seguinte estrutura:

```
type(optional scope): subject
```

Os tipos disponíveis são:

- `build`: mudanças no processo de build ou em dependências externas
- `docs`: mudanças na documentação do projeto
- `feat`: mudanças que introduzem uma nova funcionalidade
- `fix`: mudanças que resolvem um problema ou bug
- `perf`: mudanças para melhorias de performance
- `refactor`: refatorações que não adicionam funcionalidades nem corrigem
  problemas
- `style`: mudanças de estilo de código e lint
- `revert`: reversões a commits anteriores

> Exemplos: `feat: save highest score locally` e `fix(home): show the correct highest score`

### Issues

As tasks do projeto serão organizadas em issues, que facilitam o desenvolvimento
no GitHub por permitirem adicionar descrições, criar discussões e atribuir tasks
para determinados participantes. Além disso, elas ficam centralizadas no próprio
repositório e são de simples administração.

### Pull requests

Pull requests devem ser direcionados à branch `canary`. É recomendado que eles
contenham uma pequena descrição do que foi modificado, na seguinte estrutura:

- Exemplos de títulos

  - `Feature: <funcionalidade adicionada>`
  - `Fix: <fix realizado>`
  - `Docs: <mudanças na documentação>`
  - ...

- Exemplo de corpo

  ```md
  ### Features

  -

  ### Fixes

  -

  ### Refactoring

  -

  ### Docs

  -
  ```

  > Fique à vontade para omitir campos que não foram modificados. Por exemplo,
  > se não houve mudanças em documentação, não é preciso incluir a seção `Docs`.

### Releases

Quando uma versão do projeto for finalizada, um pull request da branch `canary`
para a branch `main` será realizado, atualizando a branch principal com a versão
mais atualizada do projeto. Após o merge, é interessante criar uma [release no
GitHub](https://github.com/diego-aquino/flap-plp/releases) indicando a versão do
projeto e as funcionalidades e mudanças trazidas por ela.

## Recomendações

- Ao realizar modificações, tente mantê-las relacionadas ao seu propósito no
  momento. Se você está implementando uma funcionalidade X, tente não modificar
  arquivos completamente desconectados dela, para evitar conflitos com commits
  de outras pessoas que também os tenham modificado. Caso perceba um problema
  não crítico, salve-o como [uma
  issue](https://github.com/diego-aquino/flap-plp/issues/new) e aborde-o em uma
  branch dedicada posteriormente.
