module Fable.Helpers.Virtualdom

#r "../node_modules/fable-core/Fable.Core.dll"

open Fable.Core

[<Import("h","virtual-dom")>]
let h(arg1: string, arg2: obj, arg3: obj[]): obj = failwith "JS only"

[<Import("diff","virtual-dom")>] 
let diff (tree1:obj) (tree2:obj): obj = failwith "JS only"

[<Import("patch","virtual-dom")>] 
let patch (node:obj) (patches:obj): Fable.Import.Browser.Node = failwith "JS only"

[<Import("create","virtual-dom")>]
let createElement (e:obj): Fable.Import.Browser.Node = failwith "JS only"

module Html =
    [<AutoOpen>]
    module Types =
        type MouseEvent =
            {
                altKey: bool
                screenX: int
                screenY: int
            }
        type KeyboardEvent =
            {
                code: string
                keyCode: int
            }

        type MouseEventHandler = string*(MouseEvent -> unit)
        type KeyboardEventHandler = string*(KeyboardEvent -> unit)
        type EventHandler = string*(obj -> unit)

        type EventHandlerBinding =
            | MouseEventHandler of MouseEventHandler
            | KeyboardEventHandler of KeyboardEventHandler
            | EventHandler of EventHandler

        type Style = (string*string) list

        type KeyValue = string*string

        type Attribute =
        | EventHandlerBinding of EventHandlerBinding
        | Style of Style
        | Property of KeyValue
        | Attribute of KeyValue

        type Element = string * Attribute list
        /// A Node in Html have the following forms
        type VoidElement = string * Attribute list
        type Node =
        /// A regular html element that can contain a list of other nodes
        | Element of Element * Node list
        /// A void element is one that can't have content, like link, br, hr, meta
        /// See: https://dev.w3.org/html5/html-author/#void
        | VoidElement of VoidElement
        /// A text value for a node
        | Text of string
        /// Whitespace for formatting
        | WhiteSpace of string

    [<AutoOpen>]
    module Tags =
        let inline elem tagName attrs children = Element((tagName, attrs), children)
        let inline voidElem tagName attrs = VoidElement(tagName, attrs)

        let inline whiteSpace x = WhiteSpace x
        let inline text x = Text x

        // Elements - list of elements here: https://developer.mozilla.org/en-US/docs/Web/HTML/Element
        // Void elements
        let inline br x = voidElem "br" x
        let inline area x = voidElem "area" x
        let inline baseHtml x = voidElem "base" x
        let inline col x = voidElem "col" x
        let inline embed x = voidElem "embed" x
        let inline hr x = voidElem "hr" x
        let inline img x = voidElem "img" x
        let inline input x = voidElem "input" x
        let inline link x = voidElem "link" x
        let inline meta x = voidElem "meta" x
        let inline param x = voidElem "param" x
        let inline source x = voidElem "source" x
        let inline track x = voidElem "track" x
        let inline wbr x = voidElem "wbr" x

        // Metadata
        let inline head attrs children = elem "head" attrs children
        let inline style attrs children = elem "style" attrs children
        let inline title attrs children = elem "title" attrs children

        // Content sectioning
        let inline address attrs children = elem "address" attrs children
        let inline article attrs children = elem "article" attrs children
        let inline aside attrs children = elem "aside" attrs children
        let inline footer attrs children = elem "footer" attrs children
        let inline header attrs children = elem "header" attrs children
        let inline h1 attrs children = elem "h1" attrs children
        let inline h2 attrs children = elem "h2" attrs children
        let inline h3 attrs children = elem "h3" attrs children
        let inline h4 attrs children = elem "h4" attrs children
        let inline h5 attrs children = elem "h5" attrs children
        let inline h6 attrs children = elem "h6" attrs children
        let inline hgroup attrs children = elem "hgroup" attrs children
        let inline nav attrs children = elem "nav" attrs children

        // Text content
        let inline dd attrs children = elem "dd" attrs children
        let inline div attrs children = elem "div" attrs children
        let inline dl attrs children = elem "dl" attrs children
        let inline dt attrs children = elem "dt" attrs children
        let inline figcaption attrs children = elem "figcaption" attrs children
        let inline figure attrs children = elem "figure" attrs children
        let inline li attrs children = elem "li" attrs children
        let inline main attrs children = elem "main" attrs children
        let inline ol attrs children = elem "ol" attrs children
        let inline p attrs children = elem "p" attrs children
        let inline pre attrs children = elem "pre" attrs children
        let inline section attrs children = elem "section" attrs children
        let inline ul attrs children = elem "ul" attrs children

        // Inline text semantics
        let inline a attrs children = elem "a" attrs children
        let inline abbr attrs children = elem "abbr" attrs children
        let inline b attrs children = elem "b" attrs children
        let inline bdi attrs children = elem "bdi" attrs children
        let inline bdo attrs children = elem "bdo" attrs children
        let inline cite attrs children = elem "cite" attrs children
        let inline code attrs children = elem "code" attrs children
        let inline data attrs children = elem "data" attrs children
        let inline dfn attrs children = elem "dfn" attrs children
        let inline em attrs children = elem "em" attrs children
        let inline i attrs children = elem "i" attrs children
        let inline kbd attrs children = elem "kbd" attrs children
        let inline mark attrs children = elem "mark" attrs children
        let inline q attrs children = elem "q" attrs children
        let inline rp attrs children = elem "rp" attrs children
        let inline rt attrs children = elem "rt" attrs children
        let inline rtc attrs children = elem "rtc" attrs children
        let inline ruby attrs children = elem "ruby" attrs children
        let inline s attrs children = elem "s" attrs children
        let inline samp attrs children = elem "samp" attrs children
        let inline small attrs children = elem "small" attrs children
        let inline span attrs children = elem "span" attrs children
        let inline strong attrs children = elem "strong" attrs children
        let inline sub attrs children = elem "sub" attrs children
        let inline sup attrs children = elem "sup" attrs children
        let inline time attrs children = elem "time" attrs children
        let inline u attrs children = elem "u" attrs children
        let inline var attrs children = elem "var" attrs children

        // Image and multimedia
        let inline audio attrs children = elem "audio" attrs children
        let inline map attrs children = elem "map" attrs children
        let inline video attrs children = elem "video" attrs children

        // Embedded content
        let inline objectHtml attrs children = elem "object" attrs children

        // Demarcasting edits
        let inline del attrs children = elem "del" attrs children
        let inline ins attrs children = elem "ins" attrs children

        // Table content
        let inline caption attrs children = elem "caption" attrs children
        let inline colgroup attrs children = elem "colgroup" attrs children
        let inline table attrs children = elem "table" attrs children
        let inline tbody attrs children = elem "tbody" attrs children
        let inline td attrs children = elem "td" attrs children
        let inline tfoot attrs children = elem "tfoot" attrs children
        let inline th attrs children = elem "th" attrs children
        let inline thead attrs children = elem "thead" attrs children
        let inline tr attrs children = elem "tr" attrs children

        // Forms
        let inline button attrs children = elem "button" attrs children
        let inline datalist attrs children = elem "datalist" attrs children
        let inline fieldset attrs children = elem "fieldset" attrs children
        let inline form attrs children = elem "form" attrs children
        let inline label attrs children = elem "label" attrs children
        let inline legend attrs children = elem "legend" attrs children
        let inline meter attrs children = elem "meter" attrs children
        let inline optgroup attrs children = elem "optgroup" attrs children
        let inline option attrs children = elem "option" attrs children
        let inline output attrs children = elem "output" attrs children
        let inline progress attrs children = elem "progress" attrs children
        let inline select attrs children = elem "select" attrs children
        let inline textarea attrs children = elem "textarea" attrs children

        // Interactive elements
        let inline details attrs children = elem "details" attrs children
        let inline dialog attrs children = elem "dialog" attrs children
        let inline menu attrs children = elem "menu" attrs children
        let inline menuitem attrs children = elem "menuitem" attrs children
        let inline summary attrs children = elem "summary" attrs children

    [<AutoOpen>]
    module Attributes =
        let inline attribute key value = Attribute.Attribute (key,value)
        let inline property key value = Attribute.Property (key,value)

    [<AutoOpen>]
    module Events =
        let inline onMouseEvent eventType f = EventHandlerBinding (MouseEventHandler (eventType, f))

        let inline onMouseClick x = onMouseEvent "onclick" x
        let inline onContextMenu x = onMouseEvent "oncontextmenu" x
        let inline onDblClick x = onMouseEvent "ondblclick" x
        let inline onMouseDown x = onMouseEvent "onmousedown" x
        let inline onMouseEnter x = onMouseEvent "onmouseenter" x
        let inline onMouseLeave x = onMouseEvent "onmouseleave" x
        let inline onMouseMove x = onMouseEvent "onmousemove" x
        let inline onMouseOut x = onMouseEvent "onmouseout" x
        let inline onMouseOver x = onMouseEvent "onmouseover" x
        let inline onMouseUp x = onMouseEvent "onmouseup" x
        let inline onShow x = onMouseEvent "onshow" x
        let inline onKeyboardEvent eventType f = EventHandlerBinding (KeyboardEventHandler (eventType, f))
        let inline onKeydown x = onKeyboardEvent "onkeydown" x
        let inline onKeypress x = onKeyboardEvent "onkeypress" x
        let inline onKeyup x = onKeyboardEvent "onkeyup" x

        let inline onEvent eventType f = EventHandlerBinding (EventHandler (eventType, f))
        let inline onAbort x = onEvent "onabort" x
        let inline onAfterPrint x = onEvent "onafterprint" x
        let inline onAudioEnd x = onEvent "onaudioend" x
        let inline onAudioStart x = onEvent "onaudiostart" x
        let inline onBeforePrint x = onEvent "onbeforeprint" x
        let inline onCached x = onEvent "oncached" x
        let inline onCanPlay x = onEvent "oncanplay" x
        let inline onCanPlayThrough x = onEvent "oncanplaythrough" x
        let inline onChange x = onEvent "onchange" x
        let inline onChargingChange x = onEvent "onchargingchange" x
        let inline onChargingTimeChange x = onEvent "onchargingtimechange" x
        let inline onChecking x = onEvent "onchecking" x
        let inline onClose x = onEvent "onclose" x
        let inline onDischargingTimeChange x = onEvent "ondischargingtimechange" x
        let inline onDOMContentLoaded x = onEvent "onDOMContentLoaded" x
        let inline onDownloading x = onEvent "ondownloading" x
        let inline onDurationchange x = onEvent "ondurationchange" x
        let inline onEmptied x = onEvent "onemptied" x
        let inline onEnd x = onEvent "onend" x
        let inline onEnded x = onEvent "onended" x
        let inline onError x = onEvent "onerror" x
        let inline onCullScreenChange x = onEvent "onfullscreenchange" x
        let inline onCullScreenError x = onEvent "onfullscreenerror" x
        let inline onInput x = onEvent "oninput" x
        let inline onInvalid x = onEvent "oninvalid" x
        let inline onLanguageChange x = onEvent "onlanguagechange" x
        let inline onLevelChange x = onEvent "onlevelchange" x
        let inline onLoadedData x = onEvent "onloadeddata" x
        let inline onLoadedMetaData x = onEvent "onloadedmetadata" x
        let inline onNoUpdate x = onEvent "onnoupdate" x
        let inline onObsolete x = onEvent "onobsolete" x
        let inline onOffline x = onEvent "onoffline" x
        let inline onOnline x = onEvent "ononline" x
        let inline onOpen x = onEvent "onopen" x
        let inline onOrientationChange x = onEvent "onorientationchange" x
        let inline onPause x = onEvent "onpause" x
        let inline onPointerlockchange x = onEvent "onpointerlockchange" x
        let inline onPointerlockerror x = onEvent "onpointerlockerror" x
        let inline onPlay x = onEvent "onplay" x
        let inline onPlaying x = onEvent "onplaying" x
        let inline onRateChange x = onEvent "onratechange" x
        let inline onReadyStateChange x = onEvent "onreadystatechange" x
        let inline onReset x = onEvent "onreset" x
        let inline onSeeked x = onEvent "onseeked" x
        let inline onSeeking x = onEvent "onseeking" x
        let inline onSelectStart x = onEvent "onselectstart" x
        let inline onSelectionChange x = onEvent "onselectionchange" x
        let inline onSoundEnd x = onEvent "onsoundend" x
        let inline onSoundStart x = onEvent "onsoundstart" x
        let inline onSpeechEnd x = onEvent "onspeechend" x
        let inline onSpeechStart x = onEvent "onspeechstart" x
        let inline onStalled x = onEvent "onstalled" x
        let inline onStart x = onEvent "onstart" x
        let inline onSubmit x = onEvent "onsubmit" x
        let inline onSuccess x = onEvent "onsuccess" x
        let inline onSuspend x = onEvent "onsuspend" x
        let inline onTimeUpdate x = onEvent "ontimeupdate" x
        let inline onUpdateReady x = onEvent "onupdateready" x
        let inline onVoicesChanged x = onEvent "onvoiceschanged" x
        let inline onVisibilityChange x = onEvent "onvisibilitychange" x
        let inline onVolumeChange x = onEvent "onvolumechange" x
        let inline onVrdisplayConnected x = onEvent "onvrdisplayconnected" x
        let inline onVrdisplayDisconnected x = onEvent "onvrdisplaydisconnected" x
        let inline onVrdisplayPresentChange x = onEvent "onvrdisplaypresentchange" x
        let inline onWaiting x = onEvent "onwaiting" x

        let inline onBlur x = onEvent "onblur" x
        let inline onFocus x = onEvent "onfocus" x

open Html
open Fable.Import.Browser

[<AutoOpen>]
module App =
    type Observer<'T>(next, error, completed) =
        interface System.IObserver<'T> with
            member x.OnCompleted() = completed()
            member x.OnError(e) = error e
            member x.OnNext(v) = next v

    type AppState<'TModel, 'TMessage> = {
            Model: 'TModel
            View: ('TMessage -> unit) -> 'TModel -> 'TMessage option -> Html.Types.Node
            Update: 'TModel -> 'TMessage -> ('TModel * ((unit -> unit) list)) }


    type AppEvents<'TMessage, 'TModel> =
        | ModelChanged of 'TModel*'TModel
        | ActionReceived of 'TMessage

    type Subscriber<'TMessage, 'TModel> = AppEvents<'TMessage, 'TModel> -> unit

    type App<'TModel, 'TMessage> =
        {
            AppState: AppState<'TModel, 'TMessage>
            Node: Node option
            CurrentTree: obj option
            Subscribers: Map<string, Subscriber<'TMessage, 'TModel>>
            NodeSelector: string option
        }

    let createApp appState =
        {
            AppState = appState
            Node = None
            CurrentTree = None
            Subscribers = Map.empty
            NodeSelector = None
        }

    let withStartNode selector app = { app with NodeSelector = Some selector }
    let withSubscriber subscriberId subscriber app =
        let subsribers = app.Subscribers |> Map.add subscriberId subscriber
        { app with Subscribers = subsribers }

    type AppMessage<'TMessage> =
        | AddSubscriber of string*Subscriber<'TMessage, 'TMessage>
        | RemoveSubscriber of string
        | Message of 'TMessage

    type Renderer =
        {
            Render: Html.Types.Node -> obj
            Diff: obj -> obj -> obj
            Patch: Fable.Import.Browser.Node -> obj -> Fable.Import.Browser.Node
            CreateElement: obj -> Fable.Import.Browser.Node
        }

    let start renderer app =
        let renderTree view handler model msg =
            view handler model msg
            |> renderer.Render

        let startElem =
            match app.NodeSelector with
            | None -> document.body
            | Some sel -> document.body.querySelector(sel) :?> HTMLElement

        MailboxProcessor.Start(fun inbox ->
            let post message =
                inbox.Post (Message message)

            let notifySubscribers subs model =
                subs |> Map.iter (fun key handler -> handler model)

            let rec loop state =
                async {
                    match state.Node, state.CurrentTree with
                    | None,_ ->
                        let tree = renderTree state.AppState.View post state.AppState.Model None
                        let rootNode = renderer.CreateElement tree
                        startElem.appendChild(rootNode) |> ignore
                        return! loop {state with CurrentTree = Some tree; Node = Some rootNode}
                    | Some rootNode, Some currentTree ->
                        let! message = inbox.Receive()
                        match message with
                        | Message msg ->
                            ActionReceived msg |> (notifySubscribers state.Subscribers)
                            let (model', jsCalls) = state.AppState.Update state.AppState.Model msg
                            let tree = renderTree state.AppState.View post model' (Some msg)
                            let patches = renderer.Diff currentTree tree
                            notifySubscribers state.Subscribers (ModelChanged (model', state.AppState.Model))
                            renderer.Patch rootNode patches |> ignore
                            jsCalls |> List.iter (fun i -> i())
                            return! loop {state with AppState = {state.AppState with Model = model'}; CurrentTree = Some tree}
                        | _ -> return! loop state
                    | _ -> failwith "Shouldn't happen"
                }
            loop app)

let createTree tag attributes children =
    let toAttrs attributes =
        let atts, props = obj(), obj()
        attributes |> Seq.iter (function
            | Style style -> props?style <- createObj(unbox style)
            | Property (k,v) -> props?(k) <- v
            | Attribute (k,v) -> atts?(k) <- v
            | EventHandlerBinding binding ->
                let kv = binding?Fields?(0) |> unbox<obj[]> // Performance hack
                props?(kv.[0]) <- kv.[1])
        props?attributes <- atts
        props
    h(tag, toAttrs attributes, List.toArray children)

let rec render node =
    match node with
    | Element((tag,attrs), nodes) -> createTree tag attrs (nodes |> List.map render)
    | VoidElement (tag, attrs) -> createTree tag attrs []
    | Text str -> box(string str)
    | WhiteSpace str -> box(string str)

let renderer =
    {
        Render = render
        Diff = diff
        Patch = patch
        CreateElement = createElement
    }
