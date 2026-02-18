#r "nuget: Microsoft.Extensions.Logging.Abstractions, 10.0.0"
#r "nuget: Microsoft.Extensions.Configuration.Abstractions, 10.0.1"
#r "nuget: Microsoft.Extensions.Configuration.Json, 10.0.1"
#r "nuget: Microsoft.SemanticKernel.Abstractions, 1.70.0"
#r "nuget: Microsoft.SemanticKernel.Connectors.Google, 1.70.0-alpha"
#r "nuget: FParsec, 1.0.3"
#r "nuget: MathNet.Numerics.FSharp, 4.15.0"
#r "nuget: Microsoft.Z3, 4.11.2"
#r "nuget: FSharp.Quotations.Evaluator, 2.1.0"
#r "nuget: CsvHelper, 12.1.2"
#r "nuget: Google.GenAI, 0.11.0"
#r "nuget: Microsoft.DotNet.Interactive.Formatting, 1.0.0-beta.24568.1"
#r "nuget: Unquote, 7.0.1"
#r "nuget: Sylvia.Arithmetic, 0.2.8"
#r "nuget: Markdig, 0.44.0"
#r "nuget: Serilog.Extensions.Logging, 10.0.0"
#r "nuget: Serilog.Sinks.File, 7.0.0"

#r "../src/lang/core/Sylvia.Expressions/bin/Debug/net10.0/Expect.NETStandard.dll"
#r "../ext/FunScript/src/main/FunScript/bin/Debug/netstandard2.0/FunScript.dll"
#r "../ext/FunScript/src/main/FunScript/bin/Debug/netstandard2.0/FunScript.Interop.NETStandard.dll"
#r "../ext/FunScript/src/main/FunScript/bin/Debug/netstandard2.0/FunScript.Util.dll"
#r "../ext/FunScript/src/extra/FunScript.Bindings.JSXGraph/bin/Debug/netstandard2.0/FunScript.Bindings.Base.dll"
#r "../ext/FunScript/src/extra/FunScript.Bindings.JSXGraph/bin/Debug/netstandard2.0/FunScript.Bindings.JSXGraph.dll"
#r "../src/lang/core/Sylvia.Expressions/bin/Debug/net10.0/MathNet.Symbolics.dll"

#r "../src/lang/genai/Sylvia.GenAI.Giant/bin/Debug/net10.0/Sylvia.Runtime.dll"
#r "../src/base/Sylvia.Collections/bin/Debug/netstandard2.0/Sylvia.Collections.dll"
#r "../src/lang/core/Sylvia.Expressions/bin/Debug/net10.0/Sylvia.Expressions.dll"
#r "../src/lang/core/Sylvia.Prover/bin/Debug/net10.0/Sylvia.Prover.dll"
#r "../src/lang/genai/Sylvia.GenAI.Giant/bin/Debug/net10.0/Sylvia.CAS.Maxima.dll"
#r "../src/lang/solvers/Sylvia.Solver.Z3/bin/Debug/net10.0/Sylvia.Solver.Z3.dll"
#r "../src/data/Sylvia.Data/bin/Debug/netstandard2.0/Sylvia.Data.dll"
#r "../src/lang/visualization/Sylvia.Visualization.Html/bin/Debug/net10.0/Sylvia.Visualization.Html.dll"
#r "../src/lang/genai/Sylvia.GenAI.Gemini/bin/Debug/net10.0/Sylvia.GenAI.Gemini.dll"
#r "../src/lang/genai/Sylvia.GenAI.Giant/bin/Debug/net10.0/Sylvia.GenAI.Giant.dll"

#nowarn "3391"

open System

open Google.GenAI.Types
open Microsoft.DotNet.Interactive.Formatting
open Markdig

open Sylvia
open Sylvia.CAS
open Sylvia.GenAI.Gemini    
open Sylvia.GenAI.Giant

Runtime.WithFileLogging("Sylvia", "Notebook", true, Runtime.CurentDirectory)

ModelConversation.config <- Runtime.LoadConfigFile("testappsettings.json")

ImageGenerator.config <- Runtime.LoadConfigFile("testappsettings.json")

let insideVSCode = 
    let ev = System.Environment.GetEnvironmentVariables(EnvironmentVariableTarget.Process)
    ev.Contains("VSCODE_IPC_HOOK") || ev.Contains("VSCODE_HANDLES_UNCAUGHT_ERRORS")

if System.OperatingSystem.IsWindows() then 
    Maxima.init "C:\\MathTools\\maxima-5.49.0\\bin\\maxima.bat"
//else
//    Maxima.init "/usr/lib/maxima/5.47.0/binary-gcl/maxima"


Formatter.Register<Image>(
        (fun (image: Image) (writer: System.IO.TextWriter) ->
            let html =
                if image.ImageBytes <> null && image.ImageBytes.Length > 0 then
                    let base64 = System.Convert.ToBase64String(image.ImageBytes)
                    $"<img src=\"data:image/png;base64,{base64}\" />"
                elif not (System.String.IsNullOrEmpty(image.GcsUri)) then
                    $"<img src=\"{image.GcsUri}\" />"
                else
                    "<!-- No image data available -->"
            writer.Write(html)
        ),
        HtmlFormatter.MimeType
    )


Formatter.Register<LLMProof>(
    (fun (proof: LLMProof) (writer: System.IO.TextWriter) ->
        // Convert LLM-provided markdown/intuitive text to HTML (Markdig)
        let intuitionHtml =
            if System.String.IsNullOrWhiteSpace(proof.Text) then
                "<p><em>No LLM intuition available.</em></p>"
            else
                Markdown.ToHtml(proof.Text)

        // Extract Thoughts field if present
        let thoughtsHtml =
            match proof.Thoughts with
            | Some s when (not (String.IsNullOrWhiteSpace(s))) -> Markdown.ToHtml(s)
            | _ -> ""
            

        // Convert the formal Proof object to HTML-safe text and place in a scrollable code block
        let formalHtml =
            match proof.Proof with
            | Some p ->
                let encoded = System.Net.WebUtility.HtmlEncode(p.Log)
                $"<pre class=\"llmproof-formal\"><code>{encoded}</code></pre>"
            | None ->
                "<p><em>No formal proof available.</em></p>"

        // Unique id for this output cell
        let uid = System.Guid.NewGuid().ToString("N")
        let tabIntId = "llmproof-intuition-" + uid
        let tabThoughtsId = "llmproof-thoughts-" + uid

        let hasThoughts =
            match proof.Thoughts with
            | Some s when (not (String.IsNullOrWhiteSpace(s))) -> true
            | _ -> false

        // CSS for layout and tabs
        let style =
            "<style>" +
            ".llmproof-container{display:flex;flex-direction:row;gap:12px;width:100%;box-sizing:border-box;}" +
            ".llmproof-left{flex:1 1 50%;min-width:0;border:1px solid #ddd;border-radius:6px;padding:10px;background:#fff;box-shadow:0 1px 2px rgba(0,0,0,0.04);max-height:60vh;display:flex;flex-direction:column;}" +
            ".llmproof-right{flex:1 1 50%;min-width:0;border:1px solid #ddd;border-radius:6px;padding:10px;background:#fff;box-shadow:0 1px 2px rgba(0,0,0,0.04);max-height:60vh;display:flex;flex-direction:column;overflow:hidden;}" +
            ".llmproof-tab-headers{display:flex;gap:8px;margin-bottom:8px;}" +
            ".llmproof-tabbtn{background:#f6f8fa;border:1px solid #eee;padding:6px 10px;border-radius:4px;cursor:pointer;}" +
            ".llmproof-tabbtn.active{background:#0b5fff;color:#fff;}" +
            ".llmproof-left-tabpanel{display:none;flex:1 1 auto;overflow:auto;}" +
            ".llmproof-left-tabpanel.active{display:block;}" +
            ".llmproof-thoughts-pre{white-space:pre-wrap;word-break:break-word;font-family:Menlo,Consolas,monospace;background:#f7f7f9;padding:8px;border-radius:4px;border:1px solid #eee;}" +
            ".llmproof-panel h2{margin-top:0;font-size:1rem;color:#333;}" +
            "@media (max-width:700px){.llmproof-container{flex-direction:column}.llmproof-right{max-height:none}}" +
            "</style>"

        // Tab buttons for left panel
        let tabButton leftPanelTarget label active =
            let cls = if active then "llmproof-tabbtn active" else "llmproof-tabbtn"
            let js =
                "(function(){var root=document.getElementById('" + uid + "'); var panels=root.querySelectorAll('.llmproof-left-tabpanel'); panels.forEach(function(p){p.classList.remove('active')}); root.querySelector('#" + leftPanelTarget + "').classList.add('active'); var btns=root.querySelectorAll('.llmproof-tabbtn'); btns.forEach(function(b){b.classList.remove('active')}); this.classList.add('active'); }).call(this)"
            "<button class=\"" + cls + "\" onclick=\"" + js + "\">" + label + "</button>"

        let leftButtons =
            tabButton tabIntId "Intuition" true +
            (if hasThoughts then tabButton tabThoughtsId "Thoughts" false else "")

        let leftPanels =
            "<div id=\"" + tabIntId + "\" class=\"llmproof-left-tabpanel active\">" + "<h2>LLM Intuition</h2>" + intuitionHtml + "</div>" +
            (if hasThoughts then "<div id=\"" + tabThoughtsId + "\" class=\"llmproof-left-tabpanel\"><h2>LLM Thinking</h2>" + thoughtsHtml + "</div>" else "")

        let leftHtml =
            "<div class=\"llmproof-left\">" +
                "<div class=\"llmproof-tab-headers\">" + leftButtons + "</div>" +
                leftPanels + "</div>"

        let rightHtml =
            "<div class=\"llmproof-right\">" +
                "<h2>Formal Proof</h2>" + formalHtml + "</div>"

        let html = style + "<div class=\"llmproof-container\" id=\"" + uid + "\">" + leftHtml + rightHtml + "</div>"

        writer.Write(html)
    ),
    HtmlFormatter.MimeType
)

Formatter.Register<LLMModel>(
    (fun (m: LLMModel) (writer: System.IO.TextWriter) ->
        // MathJax header for LaTeX rendering
        let mathJaxHeader = 
            if not insideVSCode then "" else
                "<script type=\"text/javascript\">" +
                "(function(){ " +
                "if(typeof window === 'undefined') return; " +
                "try { " +
                "  if(window.MathJax && window.MathJax.tex && window.MathJax.tex.inlineMath) return; " +
                "} catch(e) {} " +
                "window.MathJax = { tex: { inlineMath: [['$','$']], displayMath: [['$$','$$']] }, options: { skipHtmlTags: ['script','noscript','style','textarea','pre'] } }; " +
                "if(typeof require !== 'undefined') { " +
                "  try { require(['https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js']); } catch(e) { " +
                "    var s = document.createElement('script'); s.type='text/javascript'; s.src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'; document.head.appendChild(s); } " +
                "} else { var s = document.createElement('script'); s.type='text/javascript'; s.src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'; document.head.appendChild(s); } " +
                "})();" +
                "</script>\n"

        let encode = System.Net.WebUtility.HtmlEncode
        let intuitionHtml =
            if System.String.IsNullOrWhiteSpace(m.Text) then
                "<p><em>No LLM intuition available.</em></p>"
            else
                Markdown.ToHtml(m.Text)
        let thoughtsHtml =
            match m.Thoughts with
            | Some t when not (System.String.IsNullOrWhiteSpace t) -> Markdown.ToHtml t
            | _ -> "<p><em>No LLM thoughts available.</em></p>"

        // Helper to safely extract ModelProof as string if present (handles null or empty)
        let proofTextOpt =
            try
                match box m.ModelProof with
                | null -> None
                | :? string as s when System.String.IsNullOrWhiteSpace(s) -> None
                | :? string as s -> Some s
                | _ -> None
            with _ -> None

        // If a proof is present that means the solver returned UNSAT; show proof instead of model values.
        let formalHtml, title =            
            match m.Model with
            | Some model ->
                let valuesHtml =
                    Sylvia.Z3.get_model model
                    |> Array.map (fun (name, value) ->
                        let n = encode name
                        let v = encode value
                        $"<div class=\"llmmodel-item\"><span class=\"llmmodel-name\">{n}</span>=<span class=\"llmmodel-val-inline\">{v}</span></div>")
                    |> String.concat ""
                $"<div class=\"llmmodel-values\">\n{valuesHtml}</div><div class=\"llmmodel-proof-wrap\"></div>", "SMT Model"
            | None ->
                match proofTextOpt with
                | Some proofText ->
                    let encodedProof = encode proofText
                    $"<div class=\"llmmodel-unsat\"> <strong>UNSATISFIABLE</strong></div><div class=\"llmmodel-proof-wrap\"><pre class=\"llmmodel-proof\"><code>{encodedProof}</code></pre></div>", "SMT Result"
                | None ->  $"<div class=\"llmmodel-unsat\"> <strong>UNSATISFIABLE</strong></div>", "SMT Result"

        // Unique id for this output cell
        let uid = System.Guid.NewGuid().ToString("N")
        let tabIntId = "llmmodel-intuition-" + uid
        let tabThoughtsId = "llmmodel-thoughts-" + uid

        let hasThoughts =
            match m.Thoughts with
            | Some t when not (System.String.IsNullOrWhiteSpace t) -> true
            | _ -> false

        // CSS for layout and tabs
        let style =
            "<style>" +
            ".llmmodel-container{display:flex;flex-direction:row;gap:12px;width:100%;box-sizing:border-box;}" +
            ".llmmodel-left{flex:1 1 50%;min-width:0;border:1px solid #ddd;border-radius:6px;padding:10px;background:#fff;box-shadow:0 1px 2px rgba(0,0,0,0.04);max-height:60vh;display:flex;flex-direction:column;}" +
            ".llmmodel-right{flex:1 1 50%;min-width:0;border:1px solid #ddd;border-radius:6px;padding:10px;background:#fff;box-shadow:0 1px 2px rgba(0,0,0,0.04);max-height:60vh;display:flex;flex-direction:column;overflow:hidden;}" +
            ".llmmodel-tab-headers{display:flex;gap:8px;margin-bottom:8px;}" +
            ".llmmodel-tabbtn{background:#f6f8fa;border:1px solid #eee;padding:6px 10px;border-radius:4px;cursor:pointer;}" +
            ".llmmodel-tabbtn.active{background:#0b5fff;color:#fff;}" +
            ".llmmodel-left-tabpanel{display:none;flex:1 1 auto;overflow:auto;}" +
            ".llmmodel-left-tabpanel.active{display:block;}" +
            ".llmmodel-thoughts-pre{white-space:pre-wrap;word-break:break-word;font-family:Menlo,Consolas,monospace;background:#f7f7f9;padding:8px;border-radius:4px;border:1px solid #eee;}" +
            ".llmmodel-panel h2{margin-top:0;font-size:1rem;color:#333;}" +
            ".llmmodel-values{display:flex;flex-wrap:wrap;gap:8px;margin-bottom:8px;font-family:Menlo,Consolas,\"DejaVu Sans Mono\",monospace;font-size:0.9rem;}" +
            ".llmmodel-item{background:#f6f8fa;padding:6px 8px;border-radius:4px;border:1px solid #eee;}" +
            ".llmmodel-name{color:#0b5fff;font-weight:700;margin-right:6px;}" +
            ".llmmodel-val-inline{color:#111;}" +
            ".llmmodel-proof{background:#f7f7f9;padding:10px;border-radius:4px;overflow:auto;white-space:pre;font-family:Menlo,Consolas,\"DejaVu Sans Mono\",monospace;font-size:0.9rem;border:1px solid #eee;flex:1 1 auto;}" +
            ".llmmodel-no-values{color:#666;font-style:italic;}" +
            ".llmmodel-unsat{background:#fff4f4;border:1px solid #ffd4d6;padding:8px;border-radius:4px;margin-bottom:8px;color:#a30000;}" +
            "@media (max-width:700px){.llmmodel-container{flex-direction:column}.llmmodel-right{max-height:none}}" +
            "</style>"

        // Tab buttons for left panel
        let tabButton leftPanelTarget label active =
            let cls = if active then "llmmodel-tabbtn active" else "llmmodel-tabbtn"
            let js =
                "(function(){var root=document.getElementById('" + uid + "'); var panels=root.querySelectorAll('.llmmodel-left-tabpanel'); panels.forEach(function(p){p.classList.remove('active')}); root.querySelector('#" + leftPanelTarget + "').classList.add('active'); var btns=root.querySelectorAll('.llmmodel-tabbtn'); btns.forEach(function(b){b.classList.remove('active')}); this.classList.add('active'); }).call(this)"
            "<button class=\"" + cls + "\" onclick=\"" + js + "\">" + label + "</button>"

        let leftButtons =
            tabButton tabIntId "Intuition" true +
            (if hasThoughts then tabButton tabThoughtsId "Thinking" false else "")

        let leftPanels =
            "<div id=\"" + tabIntId + "\" class=\"llmmodel-left-tabpanel active\">" + "<h2>LLM Intuition</h2>" + intuitionHtml + "</div>" + (if hasThoughts then "<div id=\"" + tabThoughtsId + "\" class=\"llmmodel-left-tabpanel\"><h2>LLM Thinking</h2>" + thoughtsHtml + "</div>" else "")

        let leftHtml =
            "<div class=\"llmmodel-left\">" +
                "<div class=\"llmmodel-tab-headers\">" + leftButtons + "</div>" + leftPanels + "</div>"

        let rightHtml =
            "<div class=\"llmmodel-right\">" +
                "<h2>" + title + "</h2>" + formalHtml + "</div>"

        let html = mathJaxHeader + style + "<div class=\"llmmodel-container\" id=\"" + uid + "\">" + leftHtml + rightHtml + "</div>"

        writer.Write(html)
    ),
    HtmlFormatter.MimeType
)
