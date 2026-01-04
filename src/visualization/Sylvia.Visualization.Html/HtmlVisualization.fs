namespace Sylvia

type IHtmlVisualization =
    abstract Draw<'a when 'a: not null>:'a->Html

[<AutoOpen>]
module HtmlVisualization =
    let draw<'a when 'a: not null> (attrs:'a) (v:IHtmlVisualization) = v.Draw attrs

