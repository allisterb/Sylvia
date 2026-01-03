namespace Sylvia

type IHtmlVisualization =
    abstract Draw:'a->Html

[<AutoOpen>]
module HtmlVisualization =
    let draw (attrs:'a) (v:IHtmlVisualization) = v.Draw attrs

