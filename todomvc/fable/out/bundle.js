/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId])
/******/ 			return installedModules[moduleId].exports;
/******/
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			exports: {},
/******/ 			id: moduleId,
/******/ 			loaded: false
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.loaded = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(0);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	
	Object.defineProperty(exports, "__esModule", {
	    value: true
	});
	exports.todoApp = exports.initModel = exports.initList = exports.Storage = exports.todoView = exports.todoMain = exports.itemList = exports.listItem = exports.todoHeader = exports.todoFooter = exports.filters = exports.filter = exports.filterToTextAndUrl = exports.todoUpdate = exports.TodoAction = exports.TodoModel = exports.Item = exports.Filter = exports.counterView = exports.counterUpdate = exports.CounterAction = exports.initCounter = undefined;
	var Filter_1;
	
	var _fableCore = __webpack_require__(1);
	
	var _FableHelpers = __webpack_require__(2);
	
	function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }
	
	var initCounter = exports.initCounter = 0;
	
	var CounterAction = exports.CounterAction = function CounterAction() {
	    _classCallCheck(this, CounterAction);
	
	    this.Case = arguments[0];
	    this.Fields = [];
	
	    for (var i = 1; i < arguments.length; i++) {
	        this.Fields[i - 1] = arguments[i];
	    }
	};
	
	_fableCore.Util.setInterfaces(CounterAction.prototype, [], "Virtualdom.CounterAction");
	
	var counterUpdate = exports.counterUpdate = function (model, action) {
	    var x;
	    return function (m) {
	        return [m, new _fableCore.List()];
	    }(action.Case === "Increment" ? (x = action.Fields[0], model + x) : (x = action.Fields[0], model - x));
	};
	
	var counterView = exports.counterView = function (handler, model) {
	    var bgColor, x;
	    return bgColor = (x = model, x > 100) ? (x = model, "red") : (x = model, x < 0) ? (x = model, "blue") : "green", new _FableHelpers.Html.Types.Node("Element", ["div", new _fableCore.List()], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Element", ["div", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Style", _fableCore.List.ofArray([["border", "1px solid blue"]])), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["onclick", function (x) {
	        handler(new CounterAction("Increment", 1));
	    }])), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["ondblclick", function (x) {
	        handler(new CounterAction("Increment", 100));
	    }]))])], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", "Increment")])), new _FableHelpers.Html.Types.Node("Element", ["div", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Style", _fableCore.List.ofArray([["background-color", bgColor], ["color", "white"]]))])], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", model.toString())])), new _FableHelpers.Html.Types.Node("Element", ["div", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Style", _fableCore.List.ofArray([["border", "1px solid green"], ["height", (70 + model).toString() + "px"]])), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["onclick", function (x) {
	        handler(new CounterAction("Decrement", 1));
	    }])), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["ondblclick", function (x) {
	        handler(new CounterAction("Decrement", 50));
	    }]))])], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", "Decrement")]))]));
	};
	
	var Filter = exports.Filter = function Filter() {
	    _classCallCheck(this, Filter);
	
	    this.Case = arguments[0];
	    this.Fields = [];
	
	    for (var i = 1; i < arguments.length; i++) {
	        this.Fields[i - 1] = arguments[i];
	    }
	};
	
	_fableCore.Util.setInterfaces(Filter.prototype, [], "Virtualdom.Filter");
	
	var Item = exports.Item = function Item($arg0, $arg1, $arg2, $arg3) {
	    _classCallCheck(this, Item);
	
	    this.Name = $arg0;
	    this.Done = $arg1;
	    this.Id = $arg2;
	    this.IsEditing = $arg3;
	};
	
	_fableCore.Util.setInterfaces(Item.prototype, [], "Virtualdom.Item");
	
	var TodoModel = exports.TodoModel = function TodoModel($arg0, $arg1, $arg2) {
	    _classCallCheck(this, TodoModel);
	
	    this.Items = $arg0;
	    this.Input = $arg1;
	    this.Filter = $arg2;
	};
	
	_fableCore.Util.setInterfaces(TodoModel.prototype, [], "Virtualdom.TodoModel");
	
	var TodoAction = exports.TodoAction = function TodoAction() {
	    _classCallCheck(this, TodoAction);
	
	    this.Case = arguments[0];
	    this.Fields = [];
	
	    for (var i = 1; i < arguments.length; i++) {
	        this.Fields[i - 1] = arguments[i];
	    }
	};
	
	_fableCore.Util.setInterfaces(TodoAction.prototype, [], "Virtualdom.TodoAction");
	
	var todoUpdate = exports.todoUpdate = function (model, msg) {
	    var checkAllWith, updateItem, model_, jsCalls, v, i, items_, f, item, maxId, item_, Id;
	    return checkAllWith = function (v) {
	        return new TodoModel(_fableCore.List.map(function (i) {
	            return new Item(i.Name, v, i.Id, i.IsEditing);
	        }, model.Items), model.Input, model.Filter);
	    }, updateItem = function (i) {
	        return function (model_1) {
	            var items_;
	            return items_ = _fableCore.List.map(function (i_) {
	                return i_.Id !== i.Id ? i_ : i;
	            }, model_1.Items), new TodoModel(items_, model_1.Input, model_1.Filter);
	        };
	    }, model_ = msg.Case === "ChangeInput" ? (v = msg.Fields[0], new TodoModel(model.Items, v, model.Filter)) : msg.Case === "MarkAsDone" ? (i = msg.Fields[0], items_ = _fableCore.List.map(function (i_) {
	        var Done;
	        return _fableCore.Util.compareTo(i_, i) !== 0 ? i_ : (Done = true, new Item(i.Name, Done, i.Id, i.IsEditing));
	    }, model.Items), new TodoModel(items_, model.Input, model.Filter)) : msg.Case === "CheckAll" ? checkAllWith(true) : msg.Case === "UnCheckAll" ? checkAllWith(false) : msg.Case === "Destroy" ? (i = msg.Fields[0], new TodoModel(_fableCore.List.filter(function (i_) {
	        return i_.Id !== i.Id;
	    }, model.Items), model.Input, model.Filter)) : msg.Case === "ToggleItem" ? function () {
	        var i, Done;
	        return i = msg.Fields[0], updateItem((Done = !i.Done, new Item(i.Name, Done, i.Id, i.IsEditing)))(model);
	    }() : msg.Case === "SetActiveFilter" ? (f = msg.Fields[0], new TodoModel(model.Items, model.Input, f)) : msg.Case === "ClearCompleted" ? new TodoModel(_fableCore.List.filter(function (i) {
	        return !i.Done;
	    }, model.Items), model.Input, model.Filter) : msg.Case === "EditItem" ? function () {
	        var i, IsEditing;
	        return i = msg.Fields[0], updateItem((IsEditing = true, new Item(i.Name, i.Done, i.Id, IsEditing)))(model);
	    }() : msg.Case === "SaveItem" ? function () {
	        var str, i, IsEditing;
	        return str = msg.Fields[1], i = msg.Fields[0], updateItem((IsEditing = false, new Item(str, i.Done, i.Id, IsEditing)))(model);
	    }() : (item = msg.Fields[0], maxId = model.Items.tail == null ? 1 : _fableCore.Seq.max(_fableCore.List.map(function (x) {
	        return x.Id;
	    }, model.Items)), item_ = (Id = maxId + 1, new Item(item.Name, item.Done, Id, item.IsEditing)), new TodoModel(_fableCore.List.ofArray([item_], model.Items), "", model.Filter)), jsCalls = msg.Case === "EditItem" ? (i = msg.Fields[0], _fableCore.List.ofArray([function (unitVar0) {
	        document.getElementById("item-" + i.Id.toString()).focus();
	    }])) : new _fableCore.List(), [model_, jsCalls];
	};
	
	var filterToTextAndUrl = exports.filterToTextAndUrl = function (_arg1) {
	    return _arg1.Case === "Completed" ? ["Completed", "completed"] : _arg1.Case === "Active" ? ["Active", "active"] : ["All", ""];
	};
	
	var filter = exports.filter = function (handler, activeFilter, f) {
	    var linkClass, patternInput, url, fText;
	    return linkClass = _fableCore.Util.compareTo(f, activeFilter) === 0 ? "selected" : "", patternInput = filterToTextAndUrl(f), url = patternInput[1], fText = patternInput[0], new _FableHelpers.Html.Types.Node("Element", ["li", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["onclick", function (_arg1) {
	        handler(new TodoAction("SetActiveFilter", f));
	    }]))])], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Element", ["a", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["href", "#/" + url]), new _FableHelpers.Html.Types.Attribute("Attribute", ["class", linkClass])])], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", fText)]))]));
	};
	
	var filters = exports.filters = function (model, handler) {
	    return new _FableHelpers.Html.Types.Node("Element", ["ul", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "filters"])])], _fableCore.List.map(function () {
	        var activeFilter;
	        return activeFilter = model.Filter, function (f) {
	            return filter(handler, activeFilter, f);
	        };
	    }(), _fableCore.List.ofArray([new Filter("All"), new Filter("Active"), new Filter("Completed")])));
	};
	
	var todoFooter = exports.todoFooter = function (model, handler) {
	    var clearVisibility, activeCount;
	    return clearVisibility = _fableCore.Seq.exists(function (i) {
	        return i.Done;
	    }, model.Items) ? "" : "none", activeCount = _fableCore.List.filter(function (i) {
	        return !i.Done;
	    }, model.Items).length.toString(), new _FableHelpers.Html.Types.Node("Element", ["footer", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "footer"]), new _FableHelpers.Html.Types.Attribute("Style", _fableCore.List.ofArray([["display", "block"]]))])], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Element", ["span", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "todo-count"])])], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Element", ["strong", new _fableCore.List()], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", activeCount)])), new _FableHelpers.Html.Types.Node("Text", " items left")])), filters(model, handler), new _FableHelpers.Html.Types.Node("Element", ["button", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "clear-completed"]), new _FableHelpers.Html.Types.Attribute("Style", _fableCore.List.ofArray([["display", clearVisibility]])), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["onclick", function (_arg1) {
	        handler(new TodoAction("ClearCompleted"));
	    }]))])], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", "Clear completed")]))]));
	};
	
	var todoHeader = exports.todoHeader = function (model, handler) {
	    return new _FableHelpers.Html.Types.Node("Element", ["header", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "header"])])], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Element", ["h1", new _fableCore.List()], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", "todos")])), new _FableHelpers.Html.Types.Node("VoidElement", ["input", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "new-todo"]), new _FableHelpers.Html.Types.Attribute("Attribute", ["id", "new-todo"]), new _FableHelpers.Html.Types.Attribute("Property", ["placeholder", "What needs to be done?"]), new _FableHelpers.Html.Types.Attribute("Property", ["value", model]), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("KeyboardEventHandler", ["onkeydown", function (x) {
	        var Name, Id;
	        x.keyCode === 13 ? handler(new TodoAction("AddItem", (Name = x.target.value, Id = 0, new Item(Name, false, Id, false)))) : null;
	    }])), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("KeyboardEventHandler", ["onkeyup", function (x) {
	        handler(new TodoAction("ChangeInput", x.target.value));
	    }]))])])]));
	};
	
	var listItem = exports.listItem = function (handler, item) {
	    var itemChecked, editClass;
	    return itemChecked = item.Done ? "true" : "", editClass = item.IsEditing ? "editing" : "", new _FableHelpers.Html.Types.Node("Element", ["li", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", (item.Done ? "completed " : " ") + editClass])])], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Element", ["div", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "view"]), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["ondblclick", function (x) {
	        handler(new TodoAction("EditItem", item));
	    }]))])], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("VoidElement", ["input", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Property", ["className", "toggle"]), new _FableHelpers.Html.Types.Attribute("Property", ["type", "checkbox"]), new _FableHelpers.Html.Types.Attribute("Property", ["checked", itemChecked]), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["onclick", function (e) {
	        handler(new TodoAction("ToggleItem", item));
	    }]))])]), new _FableHelpers.Html.Types.Node("Element", ["label", new _fableCore.List()], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", item.Name)])), new _FableHelpers.Html.Types.Node("Element", ["button", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "destroy"]), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["onclick", function (e) {
	        handler(new TodoAction("Destroy", item));
	    }]))])], new _fableCore.List())])), new _FableHelpers.Html.Types.Node("VoidElement", ["input", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "edit"]), new _FableHelpers.Html.Types.Attribute("Attribute", ["value", item.Name]), new _FableHelpers.Html.Types.Attribute("Property", ["id", "item-" + item.Id.toString()]), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("EventHandler", ["onblur", function (e) {
	        handler(new TodoAction("SaveItem", item, e.target.value));
	    }]))])])]));
	};
	
	var itemList = exports.itemList = function (handler, items, activeFilter) {
	    var filterItems;
	    return filterItems = function (i) {
	        return activeFilter.Case === "Completed" ? i.Done : activeFilter.Case === "Active" ? !i.Done : true;
	    }, new _FableHelpers.Html.Types.Node("Element", ["ul", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "todo-list"])])], _fableCore.List.map(function (item) {
	        return listItem(handler, item);
	    }, function (list) {
	        return _fableCore.List.filter(filterItems, list);
	    }(items)));
	};
	
	var todoMain = exports.todoMain = function () {
	    var allChecked, inputEl, labelEl;
	    return allChecked = false, inputEl = new _FableHelpers.Html.Types.Node("VoidElement", ["input", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Property", ["id", "toggle-all"]), new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "toggle-all"]), new _FableHelpers.Html.Types.Attribute("Property", ["type", "checkbox"]), new _FableHelpers.Html.Types.Attribute("Property", ["checked", !allChecked ? "true" : ""])])]), labelEl = new _FableHelpers.Html.Types.Node("Element", ["label", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["for", "toggle-all"])])], _fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", "Mark all as complete")])), function (itemEls) {
	        return new _FableHelpers.Html.Types.Node("Element", ["section", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "main"]), new _FableHelpers.Html.Types.Attribute("Style", _fableCore.List.ofArray([["style", "block"]]))])], _fableCore.List.ofArray([inputEl, labelEl, new _FableHelpers.Html.Types.Node("Element", ["ul", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "todo-list"])])], itemEls)]));
	    };
	}();
	
	var todoView = exports.todoView = function () {
	    var items;
	    return items = new _fableCore.List(), function (handler) {
	        return function (model) {
	            return function (msg) {
	                var header, footer, main, $target1, todo;
	                return header = todoHeader(model.Input, handler), footer = todoFooter(model, handler), main = todoMain(($target1 = function () {
	                    return items;
	                }, msg != null ? msg.Case === "AddItem" ? (todo = msg.Fields[0], items = _fableCore.List.ofArray([listItem(handler, todo)], items), items) : $target1() : $target1())), new _FableHelpers.Html.Types.Node("Element", ["section", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "todoapp"])])], _fableCore.List.ofArray([header, main, footer]));
	            };
	        };
	    };
	}();
	
	var Storage = exports.Storage = function ($exports) {
	    var STORAGE_KEY = "vdom-storage";
	
	    var fetch = $exports.fetch = function () {
	        return function (arg00) {
	            return JSON.parse(arg00);
	        }(function (_arg1) {
	            var x;
	            return _arg1 == null ? "[]" : (x = _arg1, x);
	        }(localStorage.getItem(STORAGE_KEY)));
	    };
	
	    var save = $exports.save = function (todos) {
	        localStorage.setItem(STORAGE_KEY, JSON.stringify(todos));
	    };
	
	    return $exports;
	}({});
	
	var initList = exports.initList = function () {
	    return new _fableCore.List();
	};
	
	var initModel = exports.initModel = (Filter_1 = new Filter("All"), new TodoModel(initList(), "", Filter_1));
	
	var todoApp = exports.todoApp = _FableHelpers.App.withStartNode("#todo", _FableHelpers.App.createApp(new _FableHelpers.App.AppState(initModel, todoView, function (model) {
	    return function (msg) {
	        return todoUpdate(model, msg);
	    };
	})));
	
	(function (app) {
	    return _FableHelpers.App.start(_FableHelpers.renderer, app);
	})(todoApp);


/***/ },
/* 1 */
/***/ function(module, exports, __webpack_require__) {

	var __WEBPACK_AMD_DEFINE_FACTORY__, __WEBPACK_AMD_DEFINE_ARRAY__, __WEBPACK_AMD_DEFINE_RESULT__;(function (global, factory) {
	  if (true) {
	    !(__WEBPACK_AMD_DEFINE_ARRAY__ = [exports], __WEBPACK_AMD_DEFINE_FACTORY__ = (factory), __WEBPACK_AMD_DEFINE_RESULT__ = (typeof __WEBPACK_AMD_DEFINE_FACTORY__ === 'function' ? (__WEBPACK_AMD_DEFINE_FACTORY__.apply(exports, __WEBPACK_AMD_DEFINE_ARRAY__)) : __WEBPACK_AMD_DEFINE_FACTORY__), __WEBPACK_AMD_DEFINE_RESULT__ !== undefined && (module.exports = __WEBPACK_AMD_DEFINE_RESULT__));
	  } else if (typeof exports !== "undefined") {
	    factory(exports);
	  } else {
	    var mod = {
	      exports: {}
	    };
	    factory(mod.exports);
	    global.es2015 = mod.exports;
	  }
	})(this, function (exports) {
	  "use strict";
	
	  Object.defineProperty(exports, "__esModule", {
	    value: true
	  });
	  var FSymbol = {
	    interfaces: Symbol("interfaces"),
	    typeName: Symbol("typeName")
	  };
	  exports.Symbol = FSymbol;
	  var Choice = exports.Choice = function Choice(t, d) {
	    this.Case = t;
	    this.Fields = [d];
	  };
	
	  var Util = exports.Util = {};
	  Util.__types = new Map();
	  // For legacy reasons the name is kept, but this method also adds
	  // the type name to a cache. Use it after declaration:
	  // Util.setInterfaces(Foo.prototype, ["IFoo", "IBar"], "MyModule.Foo");
	  Util.setInterfaces = function (proto, interfaces, typeName) {
	    var curInfcs = proto[FSymbol.interfaces];
	    if (Array.isArray(interfaces) && interfaces.length > 0) {
	      if (Array.isArray(curInfcs)) {
	        for (var i = 0; i < interfaces.length; i++) {
	          curInfcs.push(interfaces[i]);
	        }
	      } else {
	        proto[FSymbol.interfaces] = interfaces;
	      }
	    }
	    if (typeName) {
	      proto[FSymbol.typeName] = typeName;
	      Util.__types.set(typeName, proto.constructor);
	    }
	  };
	  Util.hasInterface = function (obj, infc) {
	    return Array.isArray(obj[FSymbol.interfaces]) && obj[FSymbol.interfaces].indexOf(infc) >= 0;
	  };
	  Util.getRestParams = function (args, idx) {
	    for (var _len = args.length, restArgs = Array(_len > idx ? _len - idx : 0), _key = idx; _key < _len; _key++) {
	      restArgs[_key - idx] = args[_key];
	    }
	    return restArgs;
	  };
	  Util.compareTo = function (x, y) {
	    function isCollectionComparable(o) {
	      return Array.isArray(o) || ArrayBuffer.isView(o) || o instanceof List || o instanceof Map || o instanceof Set;
	    }
	    function sortIfMapOrSet(o) {
	      return o instanceof Map || o instanceof Set ? Array.from(o).sort() : o;
	    }
	    if (typeof x != typeof y) {
	      return -1;
	    }
	    if (x != null && y != null && typeof x == "object" && typeof y == "object") {
	      var lengthComp;
	      if (Object.getPrototypeOf(x) != Object.getPrototypeOf(y)) {
	        return -1;
	      }
	      if (Util.hasInterface(x, "System.IComparable")) {
	        return x.compareTo(y);
	      }
	      if (isCollectionComparable(x)) {
	        lengthComp = Util.compareTo(Seq.length(x), Seq.length(y));
	        return lengthComp != 0 ? lengthComp : Seq.fold2(function (prev, v1, v2) {
	          return prev != 0 ? prev : Util.compareTo(v1, v2);
	        }, 0, sortIfMapOrSet(x), sortIfMapOrSet(y));
	      }
	      if (x instanceof Date) {
	        return x < y ? -1 : x > y ? 1 : 0;
	      }
	      var keys1 = Object.getOwnPropertyNames(x),
	          keys2 = Object.getOwnPropertyNames(y);
	      lengthComp = Util.compareTo(keys1.length, keys2.length);
	      return lengthComp != 0 ? lengthComp : Seq.fold2(function (prev, k1, k2) {
	        return prev != 0 ? prev : Util.compareTo(x[k1], y[k2]);
	      }, 0, keys1.sort(), keys2.sort());
	    }
	    return x < y ? -1 : x > y ? 1 : 0;
	  };
	  Util.createObj = function (fields) {
	    return Seq.fold(function (acc, kv) {
	      acc[kv[0]] = kv[1];
	      return acc;
	    }, {}, fields);
	  };
	  Util.toJson = function (o) {
	    function replacer(k, v) {
	      if (ArrayBuffer.isView(v)) {
	        return Array.from(v);
	      }
	      if (typeof v == "object") {
	        if (v instanceof List || v instanceof Map || v instanceof Set) {
	          throw "JSON serialization of List, Map or Set is not supported";
	        }
	        if (v[FSymbol.typeName]) {
	          var o2 = { __type: v[FSymbol.typeName] };
	          return Object.assign(o2, v);
	        }
	      }
	      return v;
	    }
	    return JSON.stringify(o, replacer);
	  };
	  Util.ofJson = function (json) {
	    function reviver(k, v) {
	      if (typeof v == "object" && v.__type) {
	        var T = Util.__types.get(v.__type);
	        if (T) {
	          delete v.__type;
	          return Object.assign(new T(), v);
	        }
	      }
	      return v;
	    }
	    return JSON.parse(json, reviver);
	  };
	
	  var TimeSpan = exports.TimeSpan = {};
	  TimeSpan.create = TimeSpan.fromTicks = function () {
	    var d = 0,
	        h = 0,
	        m = 0,
	        s = 0,
	        ms = 0;
	    switch (arguments.length) {
	      case 1:
	        // ticks
	        return arguments[0] / 10000;
	      case 3:
	        // h,m,s
	        h = arguments[0], m = arguments[1], s = arguments[2];
	        break;
	      default:
	        // d,h,m,s,ms
	        d = arguments[0], h = arguments[1], m = arguments[2], s = arguments[3], ms = arguments[4] || 0;
	        break;
	    }
	    return d * 86400000 + h * 3600000 + m * 60000 + s * 1000 + ms;
	  };
	  TimeSpan.fromDays = function (d) {
	    return TimeSpan.create(d, 0, 0, 0);
	  };
	  TimeSpan.fromHours = function (h) {
	    return TimeSpan.create(h, 0, 0);
	  };
	  TimeSpan.fromMinutes = function (m) {
	    return TimeSpan.create(0, m, 0);
	  };
	  TimeSpan.fromSeconds = function (s) {
	    return TimeSpan.create(0, 0, s);
	  };
	  TimeSpan.days = function (ts) {
	    return Math.floor(ts / 86400000);
	  };
	  TimeSpan.hours = function (ts) {
	    return Math.floor(ts % 86400000 / 3600000);
	  };
	  TimeSpan.minutes = function (ts) {
	    return Math.floor(ts % 3600000 / 60000);
	  };
	  TimeSpan.seconds = function (ts) {
	    return Math.floor(ts % 60000 / 1000);
	  };
	  TimeSpan.milliseconds = function (ts) {
	    return Math.floor(ts % 1000);
	  };
	  TimeSpan.ticks = function (ts) {
	    return ts * 10000;
	  };
	  TimeSpan.totalDays = function (ts) {
	    return ts / 86400000;
	  };
	  TimeSpan.totalHours = function (ts) {
	    return ts / 3600000;
	  };
	  TimeSpan.totalMinutes = function (ts) {
	    return ts / 60000;
	  };
	  TimeSpan.totalSeconds = function (ts) {
	    return ts / 1000;
	  };
	  TimeSpan.duration = Math.abs;
	  TimeSpan.negate = function (ts) {
	    return -ts;
	  };
	  TimeSpan.add = function (ts1, ts2) {
	    return ts1 + ts2;
	  };
	  TimeSpan.subtract = function (ts1, ts2) {
	    return ts1 - ts2;
	  };
	  TimeSpan.compareTo = TimeSpan.compare = Util.compareTo;
	
	  var FDate = {};
	  exports.Date = FDate;
	
	
	  FDate.__changeKind = function (d, kind) {
	    var d2;
	    return d.kind == kind ? d : (d2 = new Date(d.getTime()), d2.kind = kind, d2);
	  };
	  FDate.__getValue = function (d, key) {
	    return d.kind == 1 ? d['getUTC' + key]() : d['get' + key]();
	  };
	  FDate.minValue = function () {
	    return FDate.parse(-8640000000000000, 1);
	  };
	  FDate.maxValue = function () {
	    return FDate.parse(8640000000000000, 1);
	  };
	  FDate.parse = function (v, kind) {
	    var date = v == null ? new Date() : new Date(v);
	    if (isNaN(date.getTime())) {
	      throw "The string is not a valid Date.";
	    }
	    date.kind = kind || 2; // Local
	    return date;
	  };
	  FDate.create = function (year, month, day, h, m, s, ms, kind) {
	    h = h || 0, m = m || 0, s = s || 0, ms = ms || 0, kind = kind || 2;
	    var date = kind === 1 // UTC
	    ? new Date(Date.UTC(year, month - 1, day, h, m, s, ms)) : new Date(year, month - 1, day, h, m, s, ms);
	    if (isNaN(date.getTime())) {
	      throw "The parameters describe an unrepresentable Date.";
	    }
	    date.kind = kind;
	    return date;
	  };
	  FDate.now = FDate.parse;
	  FDate.utcNow = function () {
	    return FDate.parse(null, 1);
	  };
	  FDate.today = function () {
	    return FDate.date(FDate.now());
	  };
	  FDate.isLeapYear = function (year) {
	    return year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
	  };
	  FDate.daysInMonth = function (year, month) {
	    if (month == 2) {
	      return FDate.isLeapYear(year) ? 29 : 28;
	    } else {
	      return month >= 8 ? month % 2 == 0 ? 31 : 30 : month % 2 == 0 ? 30 : 31;
	    }
	  };
	  FDate.toUniversalTime = function (d) {
	    return FDate.__changeKind(d, 1);
	  };
	  FDate.toLocalTime = function (d) {
	    return FDate.__changeKind(d, 2);
	  };
	  FDate.timeOfDay = function (d) {
	    return TimeSpan.create(FDate.hour(d), FDate.minute(d), FDate.second(d));
	  };
	  FDate.date = function (d) {
	    return FDate.create(FDate.year(d), FDate.month(d), FDate.day(d), 0, 0, 0, 0, d.kind);
	  };
	  FDate.day = function (d) {
	    return FDate.__getValue(d, "Date");
	  };
	  FDate.hour = function (d) {
	    return FDate.__getValue(d, "Hours");
	  };
	  FDate.millisecond = function (d) {
	    return FDate.__getValue(d, "Milliseconds");
	  };
	  FDate.minute = function (d) {
	    return FDate.__getValue(d, "Minutes");
	  };
	  FDate.month = function (d) {
	    return FDate.__getValue(d, "Month") + 1;
	  };
	  FDate.second = function (d) {
	    return FDate.__getValue(d, "Seconds");
	  };
	  FDate.year = function (d) {
	    return FDate.__getValue(d, "FullYear");
	  };
	  FDate.ticks = FDate.toBinary = function (d) {
	    return (d.getTime() + 6.2135604e+13 /* millisecondsJSOffset */) * 10000;
	  };
	  FDate.dayOfWeek = function (d) {
	    return FDate.__getValue(d, "Day");
	  };
	  FDate.dayOfYear = function (d) {
	    var year = FDate.year(d),
	        month = FDate.month(d),
	        day = FDate.day(d);
	    for (var i = 1; i < month; i++) {
	      day += FDate.daysInMonth(year, i);
	    }
	    return day;
	  };
	  FDate.add = FDate.op_Addition = function (d, ts) {
	    return FDate.parse(d.getTime() + ts, d.kind);
	  };
	  FDate.addDays = function (d, v) {
	    return FDate.parse(d.getTime() + v * 86400000, d.kind);
	  };
	  FDate.addHours = function (d, v) {
	    return FDate.parse(d.getTime() + v * 3600000, d.kind);
	  };
	  FDate.addMinutes = function (d, v) {
	    return FDate.parse(d.getTime() + v * 60000, d.kind);
	  };
	  FDate.addSeconds = function (d, v) {
	    return FDate.parse(d.getTime() + v * 1000, d.kind);
	  };
	  FDate.addMilliseconds = function (d, v) {
	    return FDate.parse(d.getTime() + v, d.kind);
	  };
	  FDate.addTicks = function (d, v) {
	    return FDate.parse(d.getTime() + v / 10000, d.kind);
	  };
	  FDate.addYears = function (d, v) {
	    var newMonth = FDate.month(d),
	        newYear = FDate.year(d) + v,
	        daysInMonth = FDate.daysInMonth(newYear, newMonth),
	        newDay = Math.min(daysInMonth, FDate.day(d));
	    return FDate.create(newYear, newMonth, newDay, FDate.hour(d), FDate.minute(d), FDate.second(d), FDate.millisecond(d), d.kind);
	  };
	  FDate.addMonths = function (d, v) {
	    var newMonth = FDate.month(d) + v,
	        newMonth_ = 0,
	        yearOffset = 0;
	    if (newMonth > 12) {
	      newMonth_ = newMonth % 12;
	      yearOffset = Math.floor(newMonth / 12);
	      newMonth = newMonth_;
	    } else if (newMonth < 1) {
	      newMonth_ = 12 + newMonth % 12;
	      yearOffset = Math.floor(newMonth / 12) + (newMonth_ == 12 ? -1 : 0);
	      newMonth = newMonth_;
	    }
	    var newYear = FDate.year(d) + yearOffset;
	    var daysInMonth = FDate.daysInMonth(newYear, newMonth);
	    var newDay = Math.min(daysInMonth, FDate.day(d));
	    return FDate.create(newYear, newMonth, newDay, FDate.hour(d), FDate.minute(d), FDate.second(d), FDate.millisecond(d), d.kind);
	  };
	  FDate.subtract = FDate.op_Subtraction = function (d, that) {
	    return typeof that == "number" ? FDate.parse(d.getTime() - that, d.kind) : d.getTime() - that.getTime();
	  };
	  FDate.toLongDateString = function (d) {
	    return d.toDateString();
	  };
	  FDate.toShortDateString = function (d) {
	    return d.toLocaleDateString();
	  };
	  FDate.toLongTimeString = function (d) {
	    return d.toLocaleTimeString();
	  };
	  FDate.toShortTimeString = function (d) {
	    return d.toLocaleTimeString().replace(/:\d\d(?!:)/, '');
	  };
	  FDate.equals = function (d1, d2) {
	    return d1.getTime() == d2.getTime();
	  };
	  FDate.compareTo = FDate.compare = Util.compareTo;
	
	  var Timer = exports.Timer = function Timer(interval) {
	    this.interval = interval > 0 ? interval : 100;
	    this.autoReset = true;
	    this._elapsed = new Event();
	  };
	  Object.defineProperty(Timer.prototype, 'elapsed', {
	    get: function () {
	      return this._elapsed;
	    }
	  });
	  Object.defineProperty(Timer.prototype, 'enabled', {
	    get: function () {
	      return this._enabled;
	    },
	    set: function (x) {
	      if (!this._isDisposed && this._enabled != x) {
	        if (this._enabled = x) {
	          if (this.autoReset) {
	            var _this = this;
	            this._intervalId = setInterval(function () {
	              if (!_this.autoReset) {
	                _this.enabled = false;
	              }
	              _this._elapsed.trigger(new Date());
	            }, this.interval);
	          } else {
	            var _this = this;
	            this._timeoutId = setTimeout(function () {
	              _this.enabled = false;
	              _this._timeoutId = 0;
	              if (_this.autoReset) {
	                _this.enabled = true;
	              }
	              _this._elapsed.trigger(new Date());
	            }, this.interval);
	          }
	        } else {
	          if (this._timeoutId) {
	            clearTimeout(this._timeoutId);
	            this._timeoutId = 0;
	          }
	          if (this._intervalId) {
	            clearInterval(this._intervalId);
	            this._intervalId = 0;
	          }
	        }
	      }
	    }
	  });
	  Timer.prototype.dispose = Timer.prototype.close = function () {
	    this.enabled = false;
	    this._isDisposed = true;
	  };
	  Timer.prototype.start = function () {
	    this.enabled = true;
	  };
	  Timer.prototype.stop = function () {
	    this.enabled = false;
	  };
	  Util.setInterfaces(Timer.prototype, ["System.IDisposable"]);
	
	  var FString = {};
	  exports.String = FString;
	
	
	  FString.fsFormatRegExp = /(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/;
	  FString.fsFormat = function (str) {
	    function isObject(x) {
	      return x !== null && typeof x === 'object' && !(x instanceof Number) && !(x instanceof String) && !(x instanceof Boolean);
	    };
	    function formatOnce(str, rep) {
	      return str.replace(FString.fsFormatRegExp, function (_, prefix, flags, pad, precision, format) {
	        switch (format) {
	          case "f":case "F":
	            rep = rep.toFixed(precision || 6);break;
	          case "g":case "G":
	            rep = rep.toPrecision(precision);break;
	          case "e":case "E":
	            rep = rep.toExponential(precision);break;
	          case "A":
	            rep = (rep instanceof Map ? "map " : rep instanceof Set ? "set " : "") + JSON.stringify(rep, function (k, v) {
	              return v && v[Symbol.iterator] && !Array.isArray(v) && isObject(v) ? Array.from(v) : v;
	            });
	            break;
	        }
	        var plusPrefix = flags.indexOf('+') >= 0 && parseInt(rep) >= 0;
	        if (!isNaN(pad = parseInt(pad))) {
	          var ch = pad >= 0 && flags.indexOf('0') >= 0 ? '0' : ' ';
	          rep = FString.padLeft(rep, Math.abs(pad) - (plusPrefix ? 1 : 0), ch, pad < 0);
	        }
	        return prefix + (plusPrefix ? "+" + rep : rep);
	      });
	    }
	    function makeFn(str) {
	      return function (rep) {
	        var str2 = formatOnce(str, rep);
	        return FString.fsFormatRegExp.test(str2) ? makeFn(str2) : _cont(str2.replace(/%%/g, '%'));
	      };
	    }
	    var _cont;
	    return function (cont) {
	      _cont = cont;
	      return FString.fsFormatRegExp.test(str) ? makeFn(str) : _cont(str);
	    };
	  };
	  FString.formatRegExp = /\{(\d+)(,-?\d+)?(?:\:(.+?))?\}/g;
	  FString.format = function (str, args) {
	    args = Util.getRestParams(arguments, 1);
	    return str.replace(FString.formatRegExp, function (match, idx, pad, format) {
	      var rep = args[idx];
	      if (typeof rep === 'number') {
	        switch ((format || '').substring(0, 1)) {
	          case "f":case "F":
	            rep = format.length > 1 ? rep.toFixed(format.substring(1)) : rep.toFixed(2);
	            break;
	          case "g":case "G":
	            rep = format.length > 1 ? rep.toPrecision(format.substring(1)) : rep.toPrecision();
	            break;
	          case "e":case "E":
	            rep = format.length > 1 ? rep.toExponential(format.substring(1)) : rep.toExponential();
	            break;
	          case "p":case "P":
	            rep = (format.length > 1 ? (rep * 100).toFixed(format.substring(1)) : (rep * 100).toFixed(2)) + " %";
	            break;
	        }
	      } else if (rep instanceof Date) {
	        if (format.length === 1) {
	          switch (format) {
	            case "D":
	              rep = rep.toDateString();break;
	            case "T":
	              rep = rep.toLocaleTimeString();break;
	            case "d":
	              rep = rep.toLocaleDateString();break;
	            case "t":
	              rep = rep.toLocaleTimeString().replace(/:\d\d(?!:)/, '');break;
	          }
	        }
	        rep = format.replace(/\w+/g, function (match2) {
	          var rep2 = match2;
	          switch (match2.substring(0, 1)) {
	            case "y":
	              rep2 = match2.length < 4 ? FDate.year(rep) % 100 : FDate.year(rep);
	              break;
	            case "h":
	              rep2 = rep.getHours() > 12 ? FDate.hour(rep) % 12 : FDate.hour(rep);
	              break;
	            case "M":
	              rep2 = FDate.month(rep);
	              break;
	            case "d":
	              rep2 = FDate.day(rep);
	              break;
	            case "H":
	              rep2 = FDate.hour(rep);
	              break;
	            case "m":
	              rep2 = FDate.minute(rep);
	              break;
	            case "s":
	              rep2 = FDate.second(rep);
	              break;
	          }
	          if (rep2 !== match2 && rep2 < 10 && match2.length > 1) {
	            rep2 = "0" + rep2;
	          }
	          return rep2;
	        });
	      }
	      if (!isNaN(pad = parseInt((pad || '').substring(1)))) {
	        rep = FString.padLeft(rep, Math.abs(pad), ' ', pad < 0);
	      }
	      return rep;
	    });
	  };
	  FString.init = function (n, f) {
	    if (n < 0) {
	      throw "String length must be non-negative";
	    }
	    var xs = new Array(n);
	    for (var i = 0; i < n; i++) {
	      xs[i] = f(i);
	    }
	    return xs.join("");
	  };
	  FString.isNullOrEmpty = function (str) {
	    return typeof str !== "string" || str.length == 0;
	  };
	  FString.isNullOrWhiteSpace = function (str) {
	    return typeof str !== "string" || /^\s*$/.test(str);
	  };
	  FString.padLeft = function (str, len, ch, isRight) {
	    var i = -1;
	    ch = ch || ' ';
	    str = String(str);
	    len = len - str.length;
	    while (++i < len) {
	      str = isRight ? str + ch : ch + str;
	    }
	    return str;
	  };
	  FString.padRight = function (str, len, ch) {
	    return FString.padLeft(str, len, ch, true);
	  };
	  FString.replace = function (str, search, replace) {
	    return str.replace(new RegExp(FRegExp.escape(search), "g"), replace);
	  };
	  FString.replicate = function (n, x) {
	    return FString.init(n, function () {
	      return x;
	    });
	  };
	  FString.split = function (str, splitters, count, removeEmpty) {
	    count = typeof count == "number" ? count : null;
	    removeEmpty = typeof removeEmpty == "number" ? removeEmpty : null;
	    if (count < 0) {
	      throw "Count cannot be less than zero";
	    }
	    if (count === 0) {
	      return [];
	    }
	    splitters = Array.isArray(splitters) ? splitters : Util.getRestParams(arguments, 1);
	    splitters = splitters.filter(function (x) {
	      return x;
	    }).map(function (x) {
	      return FRegExp.escape(x);
	    });
	    splitters = splitters.length > 0 ? splitters : [" "];
	    var m,
	        i = 0,
	        splits = [],
	        reg = new RegExp(splitters.join("|"), "g");
	    while ((count == null || count > 1) && (m = reg.exec(str)) !== null) {
	      if (!removeEmpty || m.index - i > 0) {
	        count = count != null ? count - 1 : count;
	        splits.push(str.substring(i, m.index));
	      }
	      i = reg.lastIndex;
	    }
	    if (!removeEmpty || str.length - i > 0) splits.push(str.substring(i));
	    return splits;
	  };
	  FString.join = FString.concat = function (delimiter, xs) {
	    xs = typeof xs == "string" ? Util.getRestParams(arguments, 1) : xs;
	    return (Array.isArray(xs) ? xs : Array.from(xs)).join(delimiter);
	  };
	  FString.endsWith = function (str, search) {
	    var idx = str.lastIndexOf(search);
	    return idx >= 0 && idx == str.length - search.length;
	  };
	  FString.newGuid = function newGuid() {
	    var i,
	        random,
	        uuid = '';
	    for (i = 0; i < 32; i++) {
	      random = Math.random() * 16 | 0;
	      if (i === 8 || i === 12 || i === 16 || i === 20) {
	        uuid += '-';
	      }
	      uuid += (i === 12 ? 4 : i === 16 ? random & 3 | 8 : random).toString(16);
	    }
	    return uuid;
	  };
	
	  var FRegExp = {};
	  exports.RegExp = FRegExp;
	
	
	  FRegExp.create = function (pattern, options) {
	    var flags = "g";
	    flags += options & 1 ? "i" : "";
	    flags += options & 2 ? "m" : "";
	    return new RegExp(pattern, flags);
	  };
	  // From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
	  FRegExp.escape = function (str) {
	    return str.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, '\\$&');
	  };
	  FRegExp.unescape = function (str) {
	    return str.replace(/\\([\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, '$1');
	  };
	  FRegExp.isMatch = function (str, pattern, options) {
	    var reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options != null ? options : 0, reg) : reg = FRegExp.create(pattern, options);
	    return reg.test(str);
	  };
	  FRegExp.match = function (str, pattern, options) {
	    var reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options != null ? options : 0, reg) : reg = FRegExp.create(pattern, options);
	    return reg.exec(str);
	  };
	  FRegExp.matches = function (str, pattern, options) {
	    var reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options != null ? options : 0, reg) : reg = FRegExp.create(pattern, options);
	    if (!reg.global) {
	      throw "Non-global RegExp"; // Prevent infinite loop
	    }
	    var m,
	        matches = [];
	    while ((m = reg.exec(str)) !== null) {
	      matches.push(m);
	    }
	    return matches;
	  };
	  FRegExp.options = function (reg) {
	    var options = 256; // ECMAScript
	    options |= reg.ignoreCase ? 1 : 0;
	    options |= reg.multiline ? 2 : 0;
	    return options;
	  };
	  FRegExp.replace = function (reg, input, replacement, limit, offset) {
	    if (typeof reg == "string") {
	      var tmp = reg;
	      reg = FRegExp.create(input, limit);
	      input = tmp, limit = undefined;
	    }
	    if (typeof replacement == "function") {
	      limit = limit == null ? -1 : limit;
	      offset = offset == null ? 0 : offset;
	      var replacer = function () {
	        var res = arguments[0];
	        if (limit !== 0) {
	          limit--;
	          var match = [];
	          var len = arguments.length;
	          for (var i = 0; i < len - 2; i++) {
	            match.push(arguments[i]);
	          }
	          match.index = arguments[len - 2];
	          match.input = arguments[len - 1];
	          res = replacement(match);
	        }
	        return res;
	      };
	      return input.substring(0, offset) + input.substring(offset).replace(reg, replacer);
	    } else {
	      if (limit != null) {
	        var m;
	        offset = offset == null ? 0 : offset;
	        var sub1 = input.substring(offset);
	        var matches = FRegExp.matches(reg, sub1);
	        var sub2 = matches.length > limit ? (m = matches[limit - 1], sub1.substring(0, m.index + m[0].length)) : sub1;
	        return input.substring(0, offset) + sub2.replace(reg, replacement) + input.substring(offset + sub2.length);
	      } else {
	        return input.replace(reg, replacement);
	      }
	    }
	  };
	  FRegExp.split = function (reg, input, limit, offset) {
	    if (typeof reg == "string") {
	      var tmp = reg;
	      reg = FRegExp.create(input, limit);
	      input = tmp, limit = undefined;
	    }
	    input = offset != null ? input.substring(offset) : input;
	    return input.split(reg, limit);
	  };
	
	  var FArray = {};
	  exports.Array = FArray;
	
	
	  FArray.addRangeInPlace = function (range, xs) {
	    Seq.iter(function (x) {
	      xs.push(x);
	    }, range);
	  };
	  FArray.blit = function (source, sourceIndex, target, targetIndex, count) {
	    while (count--) {
	      target[targetIndex++] = source[sourceIndex++];
	    }
	  };
	  FArray.partition = function (f, xs) {
	    var ys = [],
	        zs = [],
	        j = 0,
	        k = 0;
	    for (var i = 0; i < xs.length; i++) {
	      if (f(xs[i])) {
	        ys[j++] = xs[i];
	      } else {
	        zs[k++] = xs[i];
	      }
	    }
	    return [ys, zs];
	  };
	  FArray.permute = function (f, xs) {
	    // Keep the type of the array
	    var ys = xs.map(function () {
	      return 0;
	    });
	    var checkFlags = new Array(xs.length);
	    for (var i = 0; i < xs.length; i++) {
	      var j = f(i);
	      if (j < 0 || j >= xs.length) {
	        throw "Not a valid permutation";
	      }
	      ys[j] = xs[i];
	      checkFlags[j] = 1;
	    }
	    for (i = 0; i < xs.length; i++) {
	      if (checkFlags[i] != 1) {
	        throw "Not a valid permutation";
	      }
	    }
	    return ys;
	  };
	  FArray.removeInPlace = function (item, xs) {
	    var i = xs.indexOf(item);
	    if (i > -1) {
	      xs.splice(i, 1);
	      return true;
	    }
	    return false;
	  };
	  FArray.setSlice = function (target, lower, upper, source) {
	    var length = (upper || target.length - 1) - lower;
	    if (ArrayBuffer.isView(target) && source.length <= length) {
	      target.set(source, lower);
	    } else {
	      for (var i = lower | 0, j = 0; j <= length; i++, j++) {
	        target[i] = source[j];
	      }
	    }
	  };
	  FArray.sortInPlaceBy = function (f, xs, dir) {
	    dir = dir || 1;
	    return xs.sort(function (x, y) {
	      x = f(x);
	      y = f(y);
	      return (x < y ? -1 : x == y ? 0 : 1) * dir;
	    });
	  };
	  FArray.unzip = function (xs) {
	    var bs = new Array(xs.length),
	        cs = new Array(xs.length);
	    for (var i = 0; i < xs.length; i++) {
	      bs[i] = xs[i][0];
	      cs[i] = xs[i][1];
	    }
	    return [bs, cs];
	  };
	  FArray.unzip3 = function (xs) {
	    var bs = new Array(xs.length),
	        cs = new Array(xs.length),
	        ds = new Array(xs.length);
	    for (var i = 0; i < xs.length; i++) {
	      bs[i] = xs[i][0];
	      cs[i] = xs[i][1];
	      ds[i] = xs[i][2];
	    }
	    return [bs, cs, ds];
	  };
	
	  var List = exports.List = function List(head, tail) {
	    this.head = head;
	    this.tail = tail;
	  };
	  List.ofArray = function (args, base) {
	    var i = args.length - 1,
	        acc = base || new List();
	    for (; i >= 0; i--) {
	      acc = new List(args[i], acc);
	    }
	    return acc;
	  };
	  Object.defineProperty(List.prototype, 'length', {
	    get: function () {
	      return Seq.fold(function (acc, x) {
	        return acc + 1;
	      }, 0, this);
	    }
	  });
	  List.prototype[Symbol.iterator] = function () {
	    var cur = this;
	    return {
	      next: function () {
	        var tmp = cur;
	        cur = cur.tail;
	        return {
	          done: tmp.tail == null,
	          value: tmp.head
	        };
	      }
	    };
	  };
	  List.append = List.prototype.append = function (xs, ys) {
	    if (ys == null) {
	      ys = xs, xs = this;
	    }
	    return Seq.fold(function (acc, x) {
	      return new List(x, acc);
	    }, ys, List.rev(xs));
	  };
	  List.choose = List.prototype.choose = function (f, xs) {
	    return List.rev(Seq.fold(function (acc, x) {
	      var y = f(x);
	      return y != null ? new List(y, acc) : acc;
	    }, new List(), xs || this));
	  };
	  List.collect = List.prototype.collect = function (f, xs) {
	    return Seq.fold(function (acc, x) {
	      return f(x).append(acc);
	    }, new List(), List.rev(xs || this));
	  };
	  List.concat = List.prototype.concat = function (xs) {
	    return List.collect(function (x) {
	      return x;
	    }, xs || this);
	  };
	  List.filter = List.prototype.filter = List.where = List.prototype.where = function (f, xs) {
	    return List.rev(Seq.fold(function (acc, x) {
	      return f(x) ? new List(x, acc) : acc;
	    }, new List(), xs || this));
	  };
	  List.init = function (n, f) {
	    if (n < 0) {
	      throw "List length must be non-negative";
	    }
	    var xs = new List();
	    for (var i = 1; i <= n; i++) {
	      xs = new List(f(n - i), xs);
	    }
	    return xs;
	  };
	  List.map = List.prototype.map = function (f, xs) {
	    return List.rev(Seq.fold(function (acc, x) {
	      return new List(f(x), acc);
	    }, new List(), xs || this));
	  };
	  List.mapi = List.prototype.mapi = function (f, xs) {
	    return List.rev(Seq.fold(function (acc, x, i) {
	      return new List(f(i, x), acc);
	    }, new List(), xs || this));
	  };
	  List.partition = List.prototype.partition = function (f, xs) {
	    return Seq.fold(function (acc, x) {
	      var lacc = acc[0],
	          racc = acc[1];
	      return f(x) ? [new List(x, lacc), racc] : [lacc, new List(x, racc)];
	    }, [new List(), new List()], List.rev(xs || this));
	  };
	  List.replicate = function (n, x) {
	    return List.init(n, function () {
	      return x;
	    });
	  };
	  List.rev = List.prototype.rev = function (xs) {
	    return Seq.fold(function (acc, x) {
	      return new List(x, acc);
	    }, new List(), xs || this);
	  };
	  List.singleton = function (x) {
	    return new List(x, new List());
	  };
	  List.slice = List.prototype.slice = function (lower, upper, xs) {
	    var noLower = lower == null,
	        noUpper = upper == null;
	    return List.rev(Seq.fold(function (acc, x, i) {
	      var within = (noLower || lower <= i) && (noUpper || i <= upper);
	      return within ? new List(x, acc) : acc;
	    }, new List(), xs || this));
	  };
	  List.unzip = List.prototype.unzip = function (xs) {
	    return Seq.foldBack(function (xy, acc) {
	      return [new List(xy[0], acc[0]), new List(xy[1], acc[1])];
	    }, xs || this, [new List(), new List()]);
	  };
	  List.unzip3 = List.prototype.unzip3 = function (xs) {
	    return Seq.foldBack(function (xyz, acc) {
	      return [new List(xyz[0], acc[0]), new List(xyz[1], acc[1]), new List(xyz[2], acc[2])];
	    }, xs || this, [new List(), new List(), new List()]);
	  };
	
	  var Seq = exports.Seq = {};
	  Seq.__failIfNone = function (res) {
	    if (res == null) {
	      throw "Seq did not contain any matching element";
	    }
	    return res;
	  };
	  Seq.toList = function (xs) {
	    return Seq.foldBack(function (x, acc) {
	      return new List(x, acc);
	    }, xs, new List());
	  };
	  Seq.ofList = function (xs) {
	    return Seq.delay(function () {
	      return Seq.unfold(function (x) {
	        return x.tail != null ? [x.head, x.tail] : null;
	      }, xs);
	    });
	  };
	  Seq.ofArray = function (xs) {
	    return Seq.delay(function () {
	      return Seq.unfold(function (i) {
	        return i < xs.length ? [xs[i], i + 1] : null;
	      }, 0);
	    });
	  };
	  Seq.append = function (xs, ys) {
	    return Seq.delay(function () {
	      var firstDone = false;
	      var iters = [xs[Symbol.iterator](), ys];
	      return Seq.unfold(function () {
	        var cur;
	        if (!firstDone) {
	          cur = iters[0].next();
	          if (!cur.done) {
	            return [cur.value, iters];
	          } else {
	            firstDone = true;
	            iters = [null, iters[1][Symbol.iterator]()];
	          }
	        }
	        cur = iters[1].next();
	        return !cur.done ? [cur.value, iters] : null;
	      }, iters);
	    });
	  };
	  Seq.average = function (xs) {
	    var count = 1;
	    var sum = Seq.reduce(function (acc, x) {
	      count++;
	      return acc + x;
	    }, xs);
	    return sum / count;
	  };
	  Seq.averageBy = function (f, xs) {
	    var count = 1;
	    var sum = Seq.reduce(function (acc, x) {
	      count++;
	      return (count === 2 ? f(acc) : acc) + f(x);
	    }, xs);
	    return sum / count;
	  };
	  Seq.countBy = function (f, xs) {
	    return Seq.map(function (kv) {
	      return [kv[0], Seq.length(kv[1])];
	    }, Seq.groupBy(f, xs));
	  };
	  Seq.concat = function (xs) {
	    return Seq.delay(function () {
	      var iter = xs[Symbol.iterator]();
	      return Seq.unfold(function (innerIter) {
	        var cur,
	            output = null,
	            hasFinished = false;
	        while (!hasFinished) {
	          if (innerIter == null) {
	            cur = iter.next();
	            if (!cur.done) {
	              innerIter = cur.value[Symbol.iterator]();
	            } else {
	              hasFinished = true;
	            }
	          } else {
	            cur = innerIter.next();
	            if (!cur.done) {
	              output = cur.value;
	              hasFinished = true;
	            } else {
	              innerIter = null;
	            }
	          }
	        }
	        return innerIter != null && output != null ? [output, innerIter] : null;
	      }, null);
	    });
	  };
	  Seq.collect = function (f, xs) {
	    return Seq.concat(Seq.map(f, xs));
	  };
	  Seq.choose = function (f, xs) {
	    var trySkipToNext = function (iter) {
	      var cur = iter.next();
	      if (!cur.done) {
	        var y = f(cur.value);
	        return y != null ? [y, iter] : trySkipToNext(iter);
	      }
	    };
	    return Seq.delay(function () {
	      return Seq.unfold(function (iter) {
	        return trySkipToNext(iter);
	      }, xs[Symbol.iterator]());
	    });
	  };
	  Seq.compareWith = function (f, xs, ys) {
	    var nonZero = Seq.tryFind(function (i) {
	      return i != 0;
	    }, Seq.map2(function (x, y) {
	      return f(x, y);
	    }, xs, ys));
	    return nonZero != null ? nonZero : Seq.length(xs) - Seq.length(ys);
	  };
	  Seq.delay = function (f) {
	    var e = {};
	    e[Symbol.iterator] = function () {
	      return f()[Symbol.iterator]();
	    };
	    return e;
	  };
	  Seq.distinctBy = function (f, xs) {
	    return Seq.choose(function (tup) {
	      return tup[0];
	    }, Seq.scan(function (tup, x) {
	      var acc = tup[1];
	      var y = f(x);
	      return acc.has(y) ? [null, acc] : [x, acc.add(y)];
	    }, [null, new Set()], xs));
	  };
	  Seq.distinct = function (xs) {
	    return Seq.distinctBy(function (x) {
	      return x;
	    }, xs);
	  };
	  Seq.empty = function () {
	    return Seq.unfold(function () {});
	  };
	  Seq.enumerateWhile = function (cond, xs) {
	    return Seq.concat(Seq.unfold(function () {
	      return cond() ? [xs, true] : null;
	    }), true);
	  };
	  Seq.enumerateThenFinally = function (xs, finalFn) {
	    return Seq.delay(function () {
	      var iter;
	      try {
	        iter = xs[Symbol.iterator]();
	      } finally {
	        finalFn();
	      }
	      return Seq.unfold(function (iter) {
	        try {
	          var cur = iter.next();
	          return !cur.done ? [cur.value, iter] : null;
	        } finally {
	          finalFn();
	        }
	      }, iter);
	    });
	  };
	  Seq.enumerateUsing = function (disp, work) {
	    var isDisposed = false;
	    var disposeOnce = function () {
	      if (!isDisposed) {
	        isDisposed = true;
	        disp.dispose();
	      }
	    };
	    try {
	      return Seq.enumerateThenFinally(work(disp), disposeOnce);
	    } finally {
	      disposeOnce();
	    }
	  };
	  Seq.exactlyOne = function (xs) {
	    var iter = xs[Symbol.iterator]();
	    var fst = iter.next();
	    if (fst.done) {
	      throw "Seq was empty";
	    }
	    var snd = iter.next();
	    if (!snd.done) {
	      throw "Seq had multiple items";
	    }
	    return fst.value;
	  };
	  Seq.exists = function (f, xs) {
	    var aux = function (iter) {
	      var cur = iter.next();
	      return !cur.done && (f(cur.value) || aux(iter));
	    };
	    return aux(xs[Symbol.iterator]());
	  };
	  Seq.exists2 = function (f, xs, ys) {
	    var aux = function (iter1, iter2) {
	      var cur1 = iter1.next(),
	          cur2 = iter2.next();
	      return !cur1.done && !cur2.done && (f(cur1.value, cur2.value) || aux(iter1, iter2));
	    };
	    return aux(xs[Symbol.iterator](), ys[Symbol.iterator]());
	  };
	  Seq.filter = Seq.where = function (f, xs) {
	    var trySkipToNext = function (iter) {
	      var cur = iter.next();
	      if (!cur.done) {
	        return f(cur.value) ? [cur.value, iter] : trySkipToNext(iter);
	      }
	    };
	    return Seq.delay(function () {
	      return Seq.unfold(trySkipToNext, xs[Symbol.iterator]());
	    });
	  };
	  Seq.fold = function (f, acc, xs) {
	    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
	      return xs.reduce(f, acc);
	    } else {
	      for (var i = 0, cur = null, iter = xs[Symbol.iterator]();; i++) {
	        cur = iter.next();
	        if (cur.done) {
	          break;
	        }
	        acc = f(acc, cur.value, i);
	      }
	      return acc;
	    }
	  };
	  Seq.foldBack = function (f, xs, acc) {
	    var ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : Array.from(xs);
	    for (var i = ar.length - 1; i >= 0; i--) {
	      acc = f(ar[i], acc, i);
	    }
	    return acc;
	  };
	  Seq.fold2 = function (f, acc, xs, ys) {
	    var iter1 = xs[Symbol.iterator](),
	        iter2 = ys[Symbol.iterator]();
	    for (var i = 0, cur1, cur2;; i++) {
	      cur1 = iter1.next();
	      cur2 = iter2.next();
	      if (cur1.done || cur2.done) {
	        break;
	      }
	      acc = f(acc, cur1.value, cur2.value, i);
	    }
	    return acc;
	  };
	  Seq.foldBack2 = function (f, xs, ys, acc) {
	    var ar1 = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : Array.from(xs);
	    var ar2 = Array.isArray(ys) || ArrayBuffer.isView(ys) ? ys : Array.from(ys);
	    for (var i = ar1.length - 1; i >= 0; i--) {
	      acc = f(ar1[i], ar2[i], acc, i);
	    }
	    return acc;
	  };
	  Seq.forall = function (f, xs) {
	    return Seq.fold(function (acc, x) {
	      return acc && f(x);
	    }, true, xs);
	  };
	  Seq.forall2 = function (f, xs, ys) {
	    return Seq.fold2(function (acc, x, y) {
	      return acc && f(x, y);
	    }, true, xs, ys);
	  };
	  Seq.groupBy = function (f, xs) {
	    return Seq.fold(function (acc, x) {
	      var k = f(x),
	          vs = acc.get(k);
	      return vs != null ? acc.set(k, new List(x, vs)) : acc.set(k, new List(x, new List()));
	    }, new Map(), xs);
	  };
	  Seq.tryHead = function (xs) {
	    var iter = xs[Symbol.iterator]();
	    var cur = iter.next();
	    return cur.done ? null : cur.value;
	  };
	  Seq.head = function (xs) {
	    return Seq.__failIfNone(Seq.tryHead(xs));
	  };
	  Seq.init = function (n, f) {
	    return Seq.delay(function () {
	      return Seq.unfold(function (i) {
	        return i < n ? [f(i), i + 1] : null;
	      }, 0);
	    });
	  };
	  Seq.initInfinite = function (f) {
	    return Seq.delay(function () {
	      return Seq.unfold(function (i) {
	        return [f(i), i + 1];
	      }, 0);
	    });
	  };
	  Seq.tryItem = function (i, xs) {
	    if (i < 0) {
	      return null;
	    } else if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
	      return i < xs.length ? xs[i] : null;
	    } else {
	      for (var j = 0, iter = xs[Symbol.iterator]();; j++) {
	        var cur = iter.next();
	        if (cur.done) {
	          return null;
	        }
	        if (j === i) {
	          return cur.value;
	        }
	      }
	    }
	  };
	  Seq.item = function (i, xs) {
	    return Seq.__failIfNone(Seq.tryItem(i, xs));
	  };
	  Seq.iter = function (f, xs) {
	    Seq.fold(function (_, x) {
	      f(x);
	    }, null, xs);
	  };
	  Seq.iter2 = function (f, xs, ys) {
	    Seq.fold2(function (_, x, y) {
	      f(x, y);
	    }, null, xs, ys);
	  };
	  Seq.iteri = function (f, xs) {
	    Seq.fold(function (_, x, i) {
	      f(i, x);
	    }, null, xs);
	  };
	  Seq.iteri2 = function (f, xs, ys) {
	    Seq.fold2(function (_, x, y, i) {
	      f(i, x, y);
	    }, null, xs, ys);
	  };
	  Seq.isEmpty = function (xs) {
	    var i = xs[Symbol.iterator]();
	    return i.next().done;
	  };
	  Seq.tryLast = function (xs) {
	    try {
	      return Seq.reduce(function (_, x) {
	        return x;
	      }, xs);
	    } catch (err) {
	      return null;
	    }
	  };
	  Seq.last = function (xs) {
	    return Seq.__failIfNone(Seq.tryLast(xs));
	  };
	  Seq.length = function (xs) {
	    return Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs.length : Seq.fold(function (acc, x) {
	      return acc + 1;
	    }, 0, xs);
	  };
	  Seq.map = function (f, xs) {
	    return Seq.delay(function () {
	      return Seq.unfold(function (iter) {
	        var cur = iter.next();
	        return !cur.done ? [f(cur.value), iter] : null;
	      }, xs[Symbol.iterator]());
	    });
	  };
	  Seq.mapi = function (f, xs) {
	    return Seq.delay(function () {
	      var i = 0;
	      return Seq.unfold(function (iter) {
	        var cur = iter.next();
	        return !cur.done ? [f(i++, cur.value), iter] : null;
	      }, xs[Symbol.iterator]());
	    });
	  };
	  Seq.map2 = function (f, xs, ys) {
	    return Seq.delay(function () {
	      var iter1 = xs[Symbol.iterator]();
	      var iter2 = ys[Symbol.iterator]();
	      return Seq.unfold(function () {
	        var cur1 = iter1.next(),
	            cur2 = iter2.next();
	        return !cur1.done && !cur2.done ? [f(cur1.value, cur2.value), null] : null;
	      });
	    });
	  };
	  Seq.mapi2 = function (f, xs, ys) {
	    return Seq.delay(function () {
	      var i = 0;
	      var iter1 = xs[Symbol.iterator]();
	      var iter2 = ys[Symbol.iterator]();
	      return Seq.unfold(function () {
	        var cur1 = iter1.next(),
	            cur2 = iter2.next();
	        return !cur1.done && !cur2.done ? [f(i++, cur1.value, cur2.value), null] : null;
	      });
	    });
	  };
	  Seq.map3 = function (f, xs, ys, zs) {
	    return Seq.delay(function () {
	      var iter1 = xs[Symbol.iterator]();
	      var iter2 = ys[Symbol.iterator]();
	      var iter3 = zs[Symbol.iterator]();
	      return Seq.unfold(function () {
	        var cur1 = iter1.next(),
	            cur2 = iter2.next(),
	            cur3 = iter3.next();
	        return !cur1.done && !cur2.done && !cur3.done ? [f(cur1.value, cur2.value, cur3.value), null] : null;
	      });
	    });
	  };
	  Seq.max = function (xs) {
	    return Seq.reduce(function (acc, x) {
	      return Math.max(acc, x);
	    }, xs);
	  };
	  Seq.maxBy = function (f, xs) {
	    return Seq.reduce(function (x, y) {
	      return f(y) > f(x) ? y : x;
	    }, xs);
	  };
	  Seq.min = function (xs) {
	    return Seq.reduce(function (acc, x) {
	      return Math.min(acc, x);
	    }, xs);
	  };
	  Seq.minBy = function (f, xs) {
	    return Seq.reduce(function (x, y) {
	      return f(y) > f(x) ? x : y;
	    }, xs);
	  };
	  Seq.pairwise = function (xs) {
	    return Seq.skip(1, Seq.scan(function (last, next) {
	      return [last[1], next];
	    }, [0, 0], xs));
	  };
	  Seq.permute = function (f, xs) {
	    var ar = Array.from(xs);
	    return Seq.ofArray(FArray.permute(f, ar));
	  };
	  Seq.rangeStep = function (first, step, last) {
	    if (step === 0) {
	      throw "Step cannot be 0";
	    }
	    return Seq.unfold(function (x) {
	      return step > 0 && x <= last || step < 0 && x >= last ? [x, x + step] : null;
	    }, first);
	  };
	  Seq.rangeChar = function (first, last) {
	    return Seq.unfold(function (x) {
	      return x <= last ? [x, String.fromCharCode(x.charCodeAt(0) + 1)] : null;
	    }, first);
	  };
	  Seq.range = function (first, last) {
	    return Seq.rangeStep(first, 1, last);
	  };
	  Seq.readonly = function (xs) {
	    return Seq.map(function (x) {
	      return x;
	    }, xs);
	  };
	  Seq.reduce = function (f, xs) {
	    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
	      return xs.reduce(f);
	    } else {
	      var iter = xs[Symbol.iterator]();
	      var cur = iter.next();
	      if (cur.done) {
	        throw "Seq was empty";
	      }
	      var acc = cur.value;
	      for (;;) {
	        cur = iter.next();
	        if (cur.done) {
	          break;
	        }
	        acc = f(acc, cur.value);
	      }
	      return acc;
	    }
	  };
	  Seq.reduceBack = function (f, xs) {
	    var ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : Array.from(xs);
	    if (ar.length === 0) {
	      throw "Seq was empty";
	    }
	    var acc = ar[ar.length - 1];
	    for (var i = ar.length - 2; i >= 0; i--) {
	      acc = f(ar[i], acc, i);
	    }
	    return acc;
	  };
	  Seq.replicate = function (n, x) {
	    return Seq.init(n, function () {
	      return x;
	    });
	  };
	  Seq.rev = function (xs) {
	    var ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs.slice(0) : Array.from(xs);
	    return ar.reverse();
	  };
	  Seq.scan = function (f, seed, xs) {
	    return Seq.delay(function () {
	      var iter = xs[Symbol.iterator]();
	      return Seq.unfold(function (acc) {
	        if (acc == null) {
	          return [seed, seed];
	        } else {
	          var cur = iter.next();
	          if (!cur.done) {
	            acc = f(acc, cur.value);
	            return [acc, acc];
	          }
	        }
	      }, null);
	    });
	  };
	  Seq.scanBack = function (f, xs, seed) {
	    return Seq.rev(Seq.scan(function (acc, x) {
	      return f(x, acc);
	    }, seed, Seq.rev(xs)));
	  };
	  Seq.singleton = function (x) {
	    return Seq.unfold(function (x) {
	      return x != null ? [x, null] : null;
	    }, x);
	  };
	  Seq.skip = function (n, xs) {
	    var e = {};
	    e[Symbol.iterator] = function () {
	      var iter = xs[Symbol.iterator]();
	      for (var i = 1; i <= n; i++) {
	        if (iter.next().done) throw "Seq has not enough elements";
	      }
	      return iter;
	    };
	    return e;
	  };
	  Seq.skipWhile = function (f, xs) {
	    return Seq.delay(function () {
	      var hasPassed = false;
	      return Seq.filter(function (x) {
	        return hasPassed || (hasPassed = !f(x));
	      }, xs);
	    });
	  };
	  Seq.sortWith = function (f, xs) {
	    var ys = Array.from(xs);
	    return Seq.ofArray(ys.sort(f));
	  };
	  Seq.sum = function (xs, add) {
	    add = add || function (x, y) {
	      return x + y;
	    };
	    return Seq.reduce(function (acc, x) {
	      return add(acc, x);
	    }, xs);
	  };
	  Seq.sumBy = function (f, xs, add) {
	    var fst = true;
	    add = add || function (x, y) {
	      return x + y;
	    };
	    return Seq.reduce(function (acc, x) {
	      acc = fst ? f(acc) : acc, fst = false;
	      return acc + f(x);
	    }, xs);
	  };
	  Seq.tail = function (xs) {
	    var iter = xs[Symbol.iterator]();
	    var cur = iter.next();
	    if (cur.done) {
	      throw "Seq was empty";
	    }
	    var e = {};
	    e[Symbol.iterator] = function () {
	      return iter;
	    };
	    return e;
	  };
	  Seq.take = function (n, xs, truncate) {
	    return Seq.delay(function () {
	      var iter = xs[Symbol.iterator]();
	      return Seq.unfold(function (i) {
	        if (i < n) {
	          var cur = iter.next();
	          if (!cur.done) {
	            return [cur.value, i + 1];
	          } else if (!truncate) {
	            throw "Seq has not enough elements";
	          }
	        }
	      }, 0);
	    });
	  };
	  Seq.truncate = function (n, xs) {
	    return Seq.take(n, xs, true);
	  };
	  Seq.takeWhile = function (f, xs) {
	    return Seq.delay(function () {
	      var iter = xs[Symbol.iterator]();
	      return Seq.unfold(function (i) {
	        var cur = iter.next();
	        if (!cur.done && f(cur.value)) {
	          return [cur.value, null];
	        }
	      }, 0);
	    });
	  };
	  Seq.tryFind = function (f, xs) {
	    for (var i = 0, iter = xs[Symbol.iterator](), cur;; i++) {
	      cur = iter.next();
	      if (cur.done) {
	        return null;
	      }
	      if (f(cur.value, i)) {
	        return cur.value;
	      }
	    }
	  };
	  Seq.find = function (f, xs) {
	    return Seq.__failIfNone(Seq.tryFind(f, xs));
	  };
	  Seq.tryFindBack = function (f, xs) {
	    var match = null;
	    for (var i = 0, iter = xs[Symbol.iterator](), cur;; i++) {
	      cur = iter.next();
	      if (cur.done) {
	        return match;
	      }
	      if (f(cur.value, i)) {
	        match = cur.value;
	      }
	    }
	  };
	  Seq.findBack = function (f, xs) {
	    return Seq.__failIfNone(Seq.tryFindBack(f, xs));
	  };
	  Seq.tryFindIndex = function (f, xs) {
	    for (var i = 0, iter = xs[Symbol.iterator](), cur;; i++) {
	      cur = iter.next();
	      if (cur.done) {
	        return null;
	      }
	      if (f(cur.value, i)) {
	        return i;
	      }
	    }
	  };
	  Seq.findIndex = function (f, xs) {
	    return Seq.__failIfNone(Seq.tryFindIndex(f, xs));
	  };
	  Seq.tryFindIndexBack = function (f, xs) {
	    var match = null;
	    for (var i = 0, iter = xs[Symbol.iterator](), cur;; i++) {
	      cur = iter.next();
	      if (cur.done) {
	        return match;
	      }
	      if (f(cur.value, i)) {
	        match = i;
	      }
	    }
	  };
	  Seq.findIndexBack = function (f, xs) {
	    return Seq.__failIfNone(Seq.tryFindIndexBack(f, xs));
	  };
	  Seq.tryPick = function (f, xs) {
	    for (var i = 0, iter = xs[Symbol.iterator](), cur;; i++) {
	      cur = iter.next();
	      if (cur.done) {
	        break;
	      }
	      var y = f(cur.value, i);
	      if (y != null) {
	        return y;
	      }
	    }
	  };
	  Seq.pick = function (f, xs) {
	    return Seq.__failIfNone(Seq.tryPick(f, xs));
	  };
	  Seq.unfold = function (f, acc) {
	    var e = {};
	    e[Symbol.iterator] = function () {
	      return {
	        next: function () {
	          var res = f(acc);
	          if (res != null) {
	            acc = res[1];
	            return { done: false, value: res[0] };
	          } else {
	            return { done: true };
	          }
	        }
	      };
	    };
	    return e;
	  };
	  Seq.zip = function (xs, ys) {
	    return Seq.map2(function (x, y) {
	      return [x, y];
	    }, xs, ys);
	  };
	  Seq.zip3 = function (xs, ys, zs) {
	    return Seq.map3(function (x, y, z) {
	      return [x, y, z];
	    }, xs, ys, zs);
	  };
	
	  var FSet = {};
	  exports.Set = FSet;
	
	
	  FSet.ofArray = function (xs) {
	    var set = new Set();
	    for (var i = 0; i < xs.length; i++) {
	      set.add(xs[i]);
	    }
	    return set;
	  };
	  FSet.ofSeq = function (xs) {
	    return Seq.fold(function (acc, x) {
	      return acc.add(x);
	    }, new Set(), xs);
	  };
	  FSet.op_Addition = FSet.union = function (set1, set2) {
	    var set = new Set(set1);
	    set2.forEach(function (x) {
	      set.add(x);
	    });
	    return set;
	  };
	  FSet.unionMany = function (sets) {
	    return Seq.fold(function (acc, s) {
	      s.forEach(function (x) {
	        acc.add(x);
	      });
	      return acc;
	    }, new Set(), sets);
	  };
	  FSet.op_Subtraction = FSet.difference = function (set1, set2) {
	    var set = new Set(set1);
	    set2.forEach(function (x) {
	      set.delete(x);
	    });
	    return set;
	  };
	  FSet.intersect = function (set1, set2) {
	    var set = new Set(set1);
	    set1.forEach(function (x) {
	      if (!set2.has(x)) set.delete(x);
	    });
	    return set;
	  };
	  FSet.intersectMany = function (sets) {
	    var ar = Array.isArray(sets) ? sets : Array.from(sets);
	    if (ar.length == 0) {
	      throw "Seq was empty";
	    }
	    var set = new Set(ar[0]);
	    Seq.iter(function (x) {
	      for (var i = 1; i < ar.length; i++) {
	        if (!ar[i].has(x)) {
	          set.delete(x);
	          break;
	        }
	      }
	    }, ar[0]);
	    return set;
	  };
	  FSet.isProperSubsetOf = FSet.isProperSubset = function (set1, set2) {
	    return Seq.forall(function (x) {
	      return set2.has(x);
	    }, set1) && Seq.exists(function (x) {
	      return !set1.has(x);
	    }, set2);
	  };
	  FSet.isSubsetOf = FSet.isSubset = function (set1, set2) {
	    return Seq.forall(function (x) {
	      return set2.has(x);
	    }, set1);
	  };
	  FSet.isProperSupersetOf = FSet.isProperSuperset = function (set1, set2) {
	    return FSet.isProperSubset(set2, set1);
	  };
	  FSet.isSupersetOf = FSet.isSuperset = function (set1, set2) {
	    return FSet.isSubset(set2, set1);
	  };
	  FSet.copyTo = function (xs, arr, arrayIndex, count) {
	    if (!arr instanceof Array) throw "Array is invalid";
	
	    count = count || arr.length;
	    var i = arrayIndex || 0;
	    var iter = xs[Symbol.iterator]();
	    while (count--) {
	      var el = iter.next();
	      if (el.done) break;
	      arr[i++] = el.value;
	    };
	  };
	  FSet.partition = function (f, xs) {
	    return Seq.fold(function (acc, x) {
	      var lacc = acc[0],
	          racc = acc[1];
	      return f(x) ? [lacc.add(x), racc] : [lacc, racc.add(x)];
	    }, [new Set(), new Set()], xs);
	  };
	  FSet.removeInPlace = function (item, xs) {
	    xs.delete(item);
	    return xs;
	  };
	  FSet.remove = function (item, xs) {
	    return FSet.removeInPlace(item, new Set(xs));
	  };
	
	  var FMap = {};
	  exports.Map = FMap;
	
	
	  FMap.ofArray = function (xs) {
	    var map = new Map();
	    for (var i = 0; i < xs.length; i++) {
	      map.set(xs[i][0], xs[i][1]);
	    }
	    return map;
	  };
	  FMap.ofSeq = function (xs) {
	    return Seq.fold(function (acc, kv) {
	      return acc.set(kv[0], kv[1]);
	    }, new Map(), xs);
	  };
	  FMap.containsValue = function (v, map) {
	    return Seq.fold(function (acc, k) {
	      return acc || map.get(k) === v;
	    }, false, map.keys());
	  };
	  FMap.exists = function (f, map) {
	    return Seq.exists(function (kv) {
	      return f(kv[0], kv[1]);
	    }, map);
	  };
	  FMap.filter = function (f, map) {
	    return Seq.fold(function (acc, kv) {
	      return f(kv[0], kv[1]) ? acc.set(kv[0], kv[1]) : acc;
	    }, new Map(), map);
	  };
	  FMap.fold = function (f, seed, map) {
	    return Seq.fold(function (acc, kv) {
	      return f(acc, kv[0], kv[1]);
	    }, seed, map);
	  };
	  FMap.foldBack = function (f, map, seed) {
	    return Seq.foldBack(function (kv, acc) {
	      return f(kv[0], kv[1], acc);
	    }, map, seed);
	  };
	  FMap.forall = function (f, map) {
	    return Seq.forall(function (kv) {
	      return f(kv[0], kv[1]);
	    }, map);
	  };
	  FMap.iter = function (f, map) {
	    return Seq.iter(function (kv) {
	      f(kv[0], kv[1]);
	    }, map);
	  };
	  FMap.map = function (f, map) {
	    return Seq.fold(function (acc, kv) {
	      return acc.set(kv[0], f(kv[0], kv[1]));
	    }, new Map(), map);
	  };
	  FMap.partition = function (f, map) {
	    return Seq.fold(function (acc, kv) {
	      var lacc = acc[0],
	          racc = acc[1],
	          k = kv[0],
	          v = kv[1];
	      return f(k, v) ? [lacc.set(k, v), racc] : [lacc, racc.set(k, v)];
	    }, [new Map(), new Map()], map);
	  };
	  FMap.findKey = function (f, map) {
	    return Seq.pick(function (kv) {
	      return f(kv[0], kv[1]) ? kv[0] : null;
	    }, map);
	  };
	  FMap.tryFindKey = function (f, map) {
	    return Seq.tryPick(function (kv) {
	      return f(kv[0], kv[1]) ? kv[0] : null;
	    }, map);
	  };
	  FMap.pick = function (f, map) {
	    return Seq.pick(function (kv) {
	      var res = f(kv[0], kv[1]);
	      return res != null ? res : null;
	    }, map);
	  };
	  FMap.removeInPlace = FSet.removeInPlace;
	  FMap.remove = function (item, map) {
	    return FMap.removeInPlace(item, new Map(map));
	  };
	  FMap.tryPick = function (f, map) {
	    return Seq.tryPick(function (kv) {
	      var res = f(kv[0], kv[1]);
	      return res != null ? res : null;
	    }, map);
	  };
	
	  var Async = exports.Async = {};
	  Async.__protectedCont = function (f) {
	    return function (ctx) {
	      if (ctx.cancelToken.isCancelled) {
	        ctx.onCancel("cancelled");
	      } else {
	        try {
	          f(ctx);
	        } catch (err) {
	          ctx.onError(err);
	        }
	      }
	    };
	  };
	  Async.bind = function (work, cont) {
	    return Async.__protectedCont(function (ctx) {
	      work({
	        onSuccess: function (x) {
	          return cont(x)(ctx);
	        },
	        onError: ctx.onError,
	        onCancel: ctx.onCancel,
	        cancelToken: ctx.cancelToken
	      });
	    });
	  };
	  Async.combine = function (work1, work2) {
	    return Async.bind(work1, function () {
	      return work2;
	    });
	  };
	  Async.delay = function (cont) {
	    return Async.__protectedCont(function (ctx) {
	      cont()(ctx);
	    });
	  };
	  Async.for = function (seq, body) {
	    var iter = seq[Symbol.iterator](),
	        cur = iter.next();
	    return Async.while(function () {
	      return !cur.done;
	    }, Async.delay(function () {
	      var res = body(cur.value);
	      cur = iter.next();
	      return res;
	    }));
	  };
	  Async.return = function (x) {
	    return Async.__protectedCont(function (ctx) {
	      ctx.onSuccess(x);
	    });
	  };
	  Async.returnFrom = function (work) {
	    return work;
	  };
	  Async.tryFinally = function (work, finalFn) {
	    return Async.__protectedCont(function (ctx) {
	      work({
	        onSuccess: function (x) {
	          finalFn();
	          ctx.onSuccess(x);
	        },
	        onError: function (x) {
	          finalFn();
	          ctx.onError(x);
	        },
	        onCancel: function (x) {
	          finalFn();
	          ctx.onCancel(x);
	        },
	        cancelToken: ctx.cancelToken
	      });
	    });
	  };
	  Async.tryWith = function (work, catchFn) {
	    return Async.__protectedCont(function (ctx) {
	      work({
	        onSuccess: ctx.onSuccess,
	        onCancel: ctx.onCancel,
	        cancelToken: ctx.cancelToken,
	        onError: function (ex) {
	          ctx.onSuccess(catchFn(ex));
	        }
	      });
	    });
	  };
	  Async.using = function (disp, cont) {
	    return Async.tryFinally(cont(disp), function () {
	      disp.dispose();
	    });
	  };
	  Async.while = function (cond, body) {
	    if (cond()) {
	      return Async.bind(body, function () {
	        return Async.while(cond, body);
	      });
	    } else {
	      return Async.return();
	    }
	  };
	  Async.zero = function () {
	    return Async.__protectedCont(function (ctx) {
	      ctx.onSuccess();
	    });
	  };
	  Async.start = Async.startImmediate = Async.startWithContinuations = function (work, onSuccess, onError, onCancel, cancelToken) {
	    if (typeof onSuccess !== "function") {
	      cancelToken = onSuccess;
	      onSuccess = null;
	    }
	    work({
	      onSuccess: onSuccess ? onSuccess : function () {},
	      onError: onError ? onError : function () {},
	      onCancel: onCancel ? onCancel : function () {},
	      cancelToken: cancelToken ? cancelToken : {}
	    });
	  };
	  Async.ignore = function (work) {
	    return Async.bind(work, function () {
	      return Async.return();
	    });
	  };
	  Object.defineProperty(Async, 'cancellationToken', {
	    get: function () {
	      return Async.__protectedCont(function (ctx) {
	        return ctx.onSuccess(ctx.cancelToken);
	      });
	    }
	  });
	  Async.fromContinuations = function (f) {
	    return Async.__protectedCont(function (ctx) {
	      return f([ctx.onSuccess, ctx.onError, ctx.onCancel]);
	    });
	  };
	  Async.startAsPromise = function (work, cancelToken) {
	    return new Promise(function (resolve, reject) {
	      Async.startWithContinuations(work, resolve, reject, reject, cancelToken ? cancelToken : {});
	    });
	  };
	  Async.awaitPromise = function (p) {
	    return Async.fromContinuations(function (conts) {
	      p.then(conts[0]).catch(function (err) {
	        (err == "cancelled" ? conts[2] : conts[1])(err);
	      });
	    });
	  };
	  Async.parallel = function (works) {
	    return Async.awaitPromise(Promise.all(Seq.map(function (w) {
	      return Async.startAsPromise(w);
	    }, works)));
	  };
	  Async.catch = function (work) {
	    return Async.__protectedCont(function (ctx) {
	      work({
	        onSuccess: function (x) {
	          ctx.onSuccess(new Choice("Choice1Of2", x));
	        },
	        onError: function (ex) {
	          ctx.onSuccess(new Choice("Choice2Of2", ex));
	        },
	        onCancel: ctx.onCancel,
	        cancelToken: ctx.cancelToken
	      });
	    });
	  };
	  Async.sleep = function (ms) {
	    return Async.__protectedCont(function (ctx) {
	      setTimeout(function () {
	        ctx.cancelToken.isCancelled ? ctx.onCancel("cancelled") : ctx.onSuccess();
	      }, ms);
	    });
	  };
	
	  var Queue = function () {};
	  Queue.prototype.add = function (it) {
	    var itCell = { value: it };
	    if (this.firstAndLast) {
	      this.firstAndLast[1].next = itCell;
	      this.firstAndLast = [this.firstAndLast[0], itCell];
	    } else {
	      this.firstAndLast = [itCell, itCell];
	    }
	  };
	  Queue.prototype.tryGet = function (it) {
	    if (this.firstAndLast) {
	      var value = this.firstAndLast[0].value;
	      if (this.firstAndLast[0].next) {
	        this.firstAndLast = [this.firstAndLast[0].next, this.firstAndLast[1]];
	      } else {
	        delete this.firstAndLast;
	      }
	      return value;
	    }
	  };
	
	  var MailboxProcessor = exports.MailboxProcessor = function (body) {
	    this.body = body;
	    this.messages = new Queue();
	  };
	  MailboxProcessor.prototype.__processEvents = function () {
	    if (this.continuation) {
	      var value = this.messages.tryGet();
	      if (value) {
	        var cont = this.continuation;
	        delete this.continuation;
	        cont(value);
	      }
	    }
	  };
	  MailboxProcessor.prototype.start = function () {
	    Async.startImmediate(this.body(this));
	  };
	  MailboxProcessor.start = function (body) {
	    var mbox = new MailboxProcessor(body);
	    mbox.start();
	    return mbox;
	  };
	  MailboxProcessor.prototype.receive = function () {
	    var _this = this;
	    return Async.fromContinuations(function (conts) {
	      if (_this.continuation) {
	        throw "Receive can only be called once!";
	      }
	      _this.continuation = conts[0];
	      _this.__processEvents();
	    });
	  };
	  MailboxProcessor.prototype.postAndAsyncReply = function (f) {
	    var result, continuation;
	    function checkCompletion() {
	      if (result && continuation) {
	        continuation(result);
	      }
	    };
	    var reply = {
	      reply: function (res) {
	        result = res;
	        checkCompletion();
	      }
	    };
	    this.messages.add(f(reply));
	    this.__processEvents();
	    return Async.fromContinuations(function (conts) {
	      continuation = conts[0];
	      checkCompletion();
	    });
	  };
	  MailboxProcessor.prototype.post = function (msg) {
	    this.messages.add(msg);
	    this.__processEvents();
	  };
	
	  var Observer = function (onNext, onError, onCompleted) {
	    this.onNext = onNext;
	    this.onError = onError || function (e) {};
	    this.onCompleted = onCompleted || function () {};
	  };
	  Util.setInterfaces(Observer.prototype, ["System.IObserver"]);
	
	  var Observable = function (subscribe) {
	    this.subscribe = subscribe;
	  };
	  Util.setInterfaces(Observable.prototype, ["System.IObservable"]);
	
	  var Obs = {};
	  exports.Observable = Obs;
	
	
	  Obs.__protect = function (f, succeed, fail) {
	    try {
	      succeed(f());
	    } catch (e) {
	      fail(e);
	    }
	  };
	  Obs.map = function (f, w) {
	    return new Observable(function (observer) {
	      return w.subscribe(new Observer(function (v) {
	        Obs.__protect(function () {
	          f(v);
	        }, observer.onNext, observer.onError);
	      }, observer.onError, observer.onCompleted));
	    });
	  };
	  Obs.choose = function (f, w) {
	    return new Observable(function (observer) {
	      return w.subscribe(new Observer(function (v) {
	        Obs.__protect(function () {
	          f(v);
	        }, function (v) {
	          if (v != null) {
	            observer.onNext(v);
	          }
	        }, observer.onError);
	      }, observer.onError, observer.onCompleted));
	    });
	  };
	  Obs.filter = function (f, w) {
	    return Obs.choose(function (x) {
	      return f(x) ? x : null;
	    }, w);
	  };
	  Obs.partition = function (f, w) {
	    return [Obs.filter(f, w), Obs.filter(function (x) {
	      return !f(x);
	    }, w)];
	  };
	  Obs.scan = function (f, state, w) {
	    return new Observable(function (observer) {
	      return w.subscribe(new Observer(function (v) {
	        Obs.__protect(function () {
	          f(state, v);
	        }, function (z) {
	          state = z;
	          observer.onNext(z);
	        }, observer.onError);
	      }, observer.onError, observer.onCompleted));
	    });
	  };
	  Obs.add = function (f, w) {
	    w.subscribe(new Observer(f));
	  };
	  Obs.subscribe = function (f, w) {
	    return w.subscribe(new Observer(f));
	  };
	  Obs.pairwise = function (w) {
	    return new Observable(function (observer) {
	      var lastArgs = null;
	      return w.subscribe(new Observer(function (args2) {
	        if (lastArgs != null) {
	          observer.onNext([lastArgs, args2]);
	        }
	        lastArgs = args2;
	      }, observer.onError, observer.onCompleted));
	    });
	  };
	  Obs.merge = function (w1, w2) {
	    return new Observable(function (observer) {
	      var stopped = false,
	          completed1 = false,
	          completed2 = false;
	      var h1 = w1.subscribe(new Observer(function (v) {
	        if (!stopped) {
	          observer.onNext(v);
	        }
	      }, function (e) {
	        if (!stopped) {
	          stopped = true;
	          observer.onError(e);
	        }
	      }, function () {
	        if (!stopped) {
	          completed1 = true;
	          if (completed2) {
	            stopped = true;
	            observer.onCompleted();
	          }
	        }
	      }));
	      var h2 = w2.subscribe(new Observer(function (v) {
	        if (!stopped) {
	          observer.onNext(v);
	        }
	      }, function (e) {
	        if (!stopped) {
	          stopped = true;
	          observer.onError(e);
	        }
	      }, function () {
	        if (!stopped) {
	          completed2 = true;
	          if (completed1) {
	            stopped = true;
	            observer.onCompleted();
	          }
	        }
	      }));
	      var disp = {
	        dispose: function () {
	          h1.dispose();
	          h2.dispose();
	        }
	      };
	      disp[FSymbol.interfaces] = ["System.IDisposable"];
	      return disp;
	    });
	  };
	  Obs.split = function (f, w) {
	    return [Obs.choose(function (v) {
	      var res = f(v);
	      return res.Case == "Choice1Of2" ? res.Fields[0] : null;
	    }, w), Obs.choose(function (v) {
	      var res = f(v);
	      return res.Case == "Choice2Of2" ? res.Fields[0] : null;
	    }, w)];
	  };
	
	  var Event = exports.Event = function (sbscrb, delegates) {
	    var _this = this;
	    this.delegates = delegates || new Array();
	
	    this.trigger = function (value) {
	      Seq.iter(function (f) {
	        f(value);
	      }, _this.delegates);
	    };
	
	    var _addHandler = function (f) {
	      _this.delegates.push(f);
	    };
	
	    var _removeHandler = function (f) {
	      var fnd = function (el, i, arr) {
	        return '' + el == '' + f; //Special dedication to Chet Husk.
	      };
	
	      var index = _this.delegates.findIndex(fnd);
	      if (index > -1) {
	        _this.delegates.splice(index, 1);
	      }
	    };
	
	    this.subscribe = function (f) {
	      var disp;
	      return _addHandler(f), disp = {
	        dispose: function () {
	          _removeHandler(f);
	        }
	      }, disp[FSymbol.interfaces] = ["System.IDisposable"], disp;
	    };
	
	    this.add = function (f) {
	      _addHandler(f);
	    };
	
	    this.addHandler = function (f) {
	      var h = function (x) {
	        return f(undefined, x);
	      };
	      _addHandler(h);
	    };
	
	    this.removeHandler = function (f) {
	      var h = function (x) {
	        return f(undefined, x);
	      };
	      _removeHandler(h);
	    };
	
	    this._subscribe = sbscrb || function (observer) {
	      var disp,
	          f = observer.onNext;
	      return _addHandler(f), disp = {
	        dispose: function () {
	          _removeHandler(f);
	        }
	      }, disp[FSymbol.interfaces] = ["System.IDisposable"], disp;
	    };
	  };
	  Object.defineProperty(Event.prototype, "publish", {
	    get: function () {
	      return this;
	    }
	  });
	
	  Event.add = function (f, w) {
	    w._subscribe(new Observer(f));
	  };
	
	  Event.map = function (f, w) {
	    var s = function (observer) {
	      w._subscribe(new Observer(function (v) {
	        Obs.__protect(function () {
	          return f(v);
	        }, observer.onNext, observer.onError);
	      }, observer.onError, observer.onCompleted));
	    };
	    return new Event(s, w.delegates);
	  };
	
	  Event.choose = function (f, w) {
	    var s = function (observer) {
	      return w._subscribe(new Observer(function (v) {
	        Obs.__protect(function () {
	          return f(v);
	        }, function (v) {
	          if (v != null) {
	            observer.onNext(v);
	          }
	        }, observer.onError);
	      }, observer.onError, observer.onCompleted));
	    };
	    return new Event(s, w.delegates);
	  };
	
	  Event.filter = function (f, w) {
	    return Event.choose(function (x) {
	      return f(x) ? x : null;
	    }, w);
	  };
	
	  Event.partition = function (f, w) {
	    return [Event.filter(f, w), Event.filter(function (x) {
	      return !f(x);
	    }, w)];
	  };
	
	  Event.scan = function (f, state, w) {
	    var s = function (observer) {
	      return w._subscribe(new Observer(function (v) {
	        Obs.__protect(function () {
	          return f(state, v);
	        }, function (z) {
	          state = z;
	          observer.onNext(z);
	        }, observer.onError);
	      }, observer.onError, observer.onCompleted));
	    };
	    return new Event(s, w.delegates);
	  };
	
	  Event.pairwise = function (w) {
	    var s = function (observer) {
	      var lastArgs = null;
	      return w._subscribe(new Observer(function (args2) {
	        if (lastArgs != null) {
	          observer.onNext([lastArgs, args2]);
	        }
	        lastArgs = args2;
	      }, observer.onError, observer.onCompleted));
	    };
	    return new Event(s, w.delegates);
	  };
	
	  Event.merge = function (w1, w2) {
	    var s = function (observer) {
	      var stopped = false,
	          completed1 = false,
	          completed2 = false;
	      var h1 = w1._subscribe(new Observer(function (v) {
	        if (!stopped) {
	          observer.onNext(v);
	        }
	      }, function (e) {
	        if (!stopped) {
	          stopped = true;
	          observer.onError(e);
	        }
	      }, function () {
	        if (!stopped) {
	          completed1 = true;
	          if (completed2) {
	            stopped = true;
	            observer.onCompleted();
	          }
	        }
	      }));
	      var h2 = w2._subscribe(new Observer(function (v) {
	        if (!stopped) {
	          observer.onNext(v);
	        }
	      }, function (e) {
	        if (!stopped) {
	          stopped = true;
	          observer.onError(e);
	        }
	      }, function () {
	        if (!stopped) {
	          completed2 = true;
	          if (completed1) {
	            stopped = true;
	            observer.onCompleted();
	          }
	        }
	      }));
	      var disp = {
	        dispose: function () {
	          h1.dispose();
	          h2.dispose();
	        }
	      };
	      disp[FSymbol.interfaces] = ["System.IDisposable"];
	      return disp;
	    };
	
	    return new Event(s, w1.delegates.concat(w2.delegates));
	  };
	
	  Event.split = function (f, w) {
	    return [Event.choose(function (v) {
	      var res = f(v);
	      return res.Case == "Choice1Of2" ? res.Fields[0] : null;
	    }, w), Event.choose(function (v) {
	      var res = f(v);
	      return res.Case == "Choice2Of2" ? res.Fields[0] : null;
	    }, w)];
	  };
	
	  var Lazy = exports.Lazy = function Lazy(factory) {
	    this.factory = factory;
	    this.isValueCreated = false;
	  };
	  Lazy.createFromValue = function (v) {
	    return new Lazy(function () {
	      return v;
	    });
	  };
	  Object.defineProperty(Lazy.prototype, 'value', {
	    get: function () {
	      if (!this.isValueCreated) {
	        this.createdValue = this.factory();
	        this.isValueCreated = true;
	      }
	      return this.createdValue;
	    }
	  });
	});


/***/ },
/* 2 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	
	Object.defineProperty(exports, "__esModule", {
	    value: true
	});
	exports.renderer = exports.render = exports.createTree = exports.App = exports.Html = undefined;
	
	var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();
	
	var _fableCore = __webpack_require__(1);
	
	var _virtualDom = __webpack_require__(3);
	
	function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }
	
	var Html = exports.Html = function ($exports) {
	    var Types = $exports.Types = function ($exports) {
	        var MouseEvent = $exports.MouseEvent = function MouseEvent($arg0, $arg1, $arg2) {
	            _classCallCheck(this, MouseEvent);
	
	            this.altKey = $arg0;
	            this.screenX = $arg1;
	            this.screenY = $arg2;
	        };
	
	        _fableCore.Util.setInterfaces(MouseEvent.prototype, [], "Fable.Helpers.Virtualdom.Html.Types.MouseEvent");
	
	        var KeyboardEvent = $exports.KeyboardEvent = function KeyboardEvent($arg0, $arg1) {
	            _classCallCheck(this, KeyboardEvent);
	
	            this.code = $arg0;
	            this.keyCode = $arg1;
	        };
	
	        _fableCore.Util.setInterfaces(KeyboardEvent.prototype, [], "Fable.Helpers.Virtualdom.Html.Types.KeyboardEvent");
	
	        var EventHandlerBinding = $exports.EventHandlerBinding = function EventHandlerBinding() {
	            _classCallCheck(this, EventHandlerBinding);
	
	            this.Case = arguments[0];
	            this.Fields = [];
	
	            for (var i = 1; i < arguments.length; i++) {
	                this.Fields[i - 1] = arguments[i];
	            }
	        };
	
	        _fableCore.Util.setInterfaces(EventHandlerBinding.prototype, [], "Fable.Helpers.Virtualdom.Html.Types.EventHandlerBinding");
	
	        var Attribute = $exports.Attribute = function Attribute() {
	            _classCallCheck(this, Attribute);
	
	            this.Case = arguments[0];
	            this.Fields = [];
	
	            for (var i = 1; i < arguments.length; i++) {
	                this.Fields[i - 1] = arguments[i];
	            }
	        };
	
	        _fableCore.Util.setInterfaces(Attribute.prototype, [], "Fable.Helpers.Virtualdom.Html.Types.Attribute");
	
	        var Node = $exports.Node = function Node() {
	            _classCallCheck(this, Node);
	
	            this.Case = arguments[0];
	            this.Fields = [];
	
	            for (var i = 1; i < arguments.length; i++) {
	                this.Fields[i - 1] = arguments[i];
	            }
	        };
	
	        _fableCore.Util.setInterfaces(Node.prototype, [], "Fable.Helpers.Virtualdom.Html.Types.Node");
	
	        return $exports;
	    }({});
	
	    var Tags = $exports.Tags = function ($exports) {
	        return $exports;
	    }({});
	
	    var Attributes = $exports.Attributes = function ($exports) {
	        return $exports;
	    }({});
	
	    var Events = $exports.Events = function ($exports) {
	        return $exports;
	    }({});
	
	    return $exports;
	}({});
	
	var App = exports.App = function ($exports) {
	    var Observer = $exports.Observer = function () {
	        function Observer(next, error, completed) {
	            _classCallCheck(this, Observer);
	
	            this.next = next;
	            this.error = error;
	            this.completed = completed;
	        }
	
	        _createClass(Observer, [{
	            key: "onCompleted",
	            value: function onCompleted() {
	                this.completed();
	            }
	        }, {
	            key: "onError",
	            value: function onError(e) {
	                this.error(e);
	            }
	        }, {
	            key: "onNext",
	            value: function onNext(v) {
	                this.next(v);
	            }
	        }]);
	
	        return Observer;
	    }();
	
	    _fableCore.Util.setInterfaces(Observer.prototype, ["System.IObserver"], "Fable.Helpers.Virtualdom.App.Observer");
	
	    var AppState = $exports.AppState = function AppState($arg0, $arg1, $arg2) {
	        _classCallCheck(this, AppState);
	
	        this.Model = $arg0;
	        this.View = $arg1;
	        this.Update = $arg2;
	    };
	
	    _fableCore.Util.setInterfaces(AppState.prototype, [], "Fable.Helpers.Virtualdom.App.AppState");
	
	    var AppEvents = $exports.AppEvents = function AppEvents() {
	        _classCallCheck(this, AppEvents);
	
	        this.Case = arguments[0];
	        this.Fields = [];
	
	        for (var i = 1; i < arguments.length; i++) {
	            this.Fields[i - 1] = arguments[i];
	        }
	    };
	
	    _fableCore.Util.setInterfaces(AppEvents.prototype, [], "Fable.Helpers.Virtualdom.App.AppEvents");
	
	    var App = $exports.App = function App($arg0, $arg1, $arg2, $arg3, $arg4) {
	        _classCallCheck(this, App);
	
	        this.AppState = $arg0;
	        this.Node = $arg1;
	        this.CurrentTree = $arg2;
	        this.Subscribers = $arg3;
	        this.NodeSelector = $arg4;
	    };
	
	    _fableCore.Util.setInterfaces(App.prototype, [], "Fable.Helpers.Virtualdom.App.App");
	
	    var createApp = $exports.createApp = function (appState) {
	        return new App(appState, null, null, new Map());
	    };
	
	    var withStartNode = $exports.withStartNode = function (selector, app) {
	        var NodeSelector;
	        return NodeSelector = selector, new App(app.AppState, app.Node, app.CurrentTree, app.Subscribers, NodeSelector);
	    };
	
	    var withSubscriber = $exports.withSubscriber = function (subscriberId, subscriber, app) {
	        var subsribers;
	        return subsribers = function (table) {
	            return new Map(table).set(subscriberId, subscriber);
	        }(app.Subscribers), new App(app.AppState, app.Node, app.CurrentTree, subsribers, app.NodeSelector);
	    };
	
	    var AppMessage = $exports.AppMessage = function AppMessage() {
	        _classCallCheck(this, AppMessage);
	
	        this.Case = arguments[0];
	        this.Fields = [];
	
	        for (var i = 1; i < arguments.length; i++) {
	            this.Fields[i - 1] = arguments[i];
	        }
	    };
	
	    _fableCore.Util.setInterfaces(AppMessage.prototype, [], "Fable.Helpers.Virtualdom.App.AppMessage");
	
	    var Renderer = $exports.Renderer = function Renderer($arg0, $arg1, $arg2, $arg3) {
	        _classCallCheck(this, Renderer);
	
	        this.Render = $arg0;
	        this.Diff = $arg1;
	        this.Patch = $arg2;
	        this.CreateElement = $arg3;
	    };
	
	    _fableCore.Util.setInterfaces(Renderer.prototype, [], "Fable.Helpers.Virtualdom.App.Renderer");
	
	    var start = $exports.start = function (renderer, app) {
	        var renderTree, startElem, matchValue, sel;
	        return renderTree = function (view) {
	            return function (handler) {
	                return function (model) {
	                    return function (msg) {
	                        return renderer.Render(view(handler)(model)(msg));
	                    };
	                };
	            };
	        }, startElem = (matchValue = app.NodeSelector, matchValue != null ? (sel = matchValue, document.body.querySelector(sel)) : document.body), _fableCore.MailboxProcessor.start(function (inbox) {
	            var post, notifySubscribers, loop;
	            return post = function (message) {
	                inbox.post(new AppMessage("Message", message));
	            }, notifySubscribers = function (subs) {
	                return function (model) {
	                    _fableCore.Map.iter(function (key, handler) {
	                        handler(model);
	                    }, subs);
	                };
	            }, loop = function (state) {
	                return function (builder_) {
	                    return builder_.delay(function (unitVar) {
	                        var matchValue, currentTree, rootNode, tree, CurrentTree, Node;
	                        return matchValue = [state.Node, state.CurrentTree], matchValue[0] != null ? matchValue[1] != null ? (currentTree = matchValue[1], rootNode = matchValue[0], builder_.bind(inbox.receive(), function (_arg1) {
	                            var message, msg, patternInput, model_, jsCalls, tree, patches, AppState_1, CurrentTree, inputRecord;
	                            return message = _arg1, message.Case === "Message" ? (msg = message.Fields[0], notifySubscribers(state.Subscribers)(new AppEvents("ActionReceived", msg)), patternInput = state.AppState.Update(state.AppState.Model)(msg), model_ = patternInput[0], jsCalls = patternInput[1], tree = renderTree(state.AppState.View)(post)(model_)(msg), patches = renderer.Diff(currentTree)(tree), notifySubscribers(state.Subscribers)(new AppEvents("ModelChanged", model_, state.AppState.Model)), renderer.Patch(rootNode)(patches), _fableCore.Seq.iter(function (i) {
	                                i();
	                            }, jsCalls), builder_.returnFrom(loop((AppState_1 = (inputRecord = state.AppState, new AppState(model_, inputRecord.View, inputRecord.Update)), CurrentTree = tree, new App(AppState_1, state.Node, CurrentTree, state.Subscribers, state.NodeSelector))))) : builder_.returnFrom(loop(state));
	                        })) : function () {
	                            throw "Shouldn't happen";
	                            return builder_.zero();
	                        }() : (tree = renderTree(state.AppState.View)(post)(state.AppState.Model)(), rootNode = renderer.CreateElement(tree), startElem.appendChild(rootNode), builder_.returnFrom(loop((CurrentTree = tree, Node = rootNode, new App(state.AppState, Node, CurrentTree, state.Subscribers, state.NodeSelector)))));
	                    });
	                }(_fableCore.Async);
	            }, loop(app);
	        });
	    };
	
	    return $exports;
	}({});
	
	var createTree = exports.createTree = function (tag, attributes, children) {
	    var toAttrs;
	    return toAttrs = function (attributes_1) {
	        var patternInput, props, atts;
	        return patternInput = [{}, {}], props = patternInput[1], atts = patternInput[0], _fableCore.Seq.iter(function (_arg1) {
	            var v, k, binding, kv, style;
	            _arg1.Case === "Property" ? (v = _arg1.Fields[0][1], k = _arg1.Fields[0][0], props[k] = v) : _arg1.Case === "Attribute" ? (v = _arg1.Fields[0][1], k = _arg1.Fields[0][0], atts[k] = v) : _arg1.Case === "EventHandlerBinding" ? (binding = _arg1.Fields[0], kv = binding.Fields[0], props[kv[0]] = kv[1]) : (style = _arg1.Fields[0], props.style = _fableCore.Util.createObj(style));
	        }, attributes_1), props.attributes = atts, props;
	    }, (0, _virtualDom.h)(tag, toAttrs(attributes), Array.from(children));
	};
	
	var render = exports.render = function (node) {
	    var tag, attrs, str, nodes;
	    return node.Case === "VoidElement" ? (tag = node.Fields[0][0], attrs = node.Fields[0][1], createTree(tag, attrs, new _fableCore.List())) : node.Case === "Text" ? (str = node.Fields[0], str) : node.Case === "WhiteSpace" ? (str = node.Fields[0], str) : (tag = node.Fields[0][0], nodes = node.Fields[1], attrs = node.Fields[0][1], createTree(tag, attrs, _fableCore.List.map(function (node_1) {
	        return render(node_1);
	    }, nodes)));
	};
	
	var renderer = exports.renderer = new App.Renderer(function (node) {
	    return render(node);
	}, function (tree1) {
	    return function (tree2) {
	        return (0, _virtualDom.diff)(tree1, tree2);
	    };
	}, function (node) {
	    return function (patches) {
	        return (0, _virtualDom.patch)(node, patches);
	    };
	}, function (e) {
	    return (0, _virtualDom.create)(e);
	});


/***/ },
/* 3 */
/***/ function(module, exports, __webpack_require__) {

	var diff = __webpack_require__(4)
	var patch = __webpack_require__(17)
	var h = __webpack_require__(26)
	var create = __webpack_require__(37)
	var VNode = __webpack_require__(28)
	var VText = __webpack_require__(29)
	
	module.exports = {
	    diff: diff,
	    patch: patch,
	    h: h,
	    create: create,
	    VNode: VNode,
	    VText: VText
	}


/***/ },
/* 4 */
/***/ function(module, exports, __webpack_require__) {

	var diff = __webpack_require__(5)
	
	module.exports = diff


/***/ },
/* 5 */
/***/ function(module, exports, __webpack_require__) {

	var isArray = __webpack_require__(6)
	
	var VPatch = __webpack_require__(7)
	var isVNode = __webpack_require__(9)
	var isVText = __webpack_require__(10)
	var isWidget = __webpack_require__(11)
	var isThunk = __webpack_require__(12)
	var handleThunk = __webpack_require__(13)
	
	var diffProps = __webpack_require__(14)
	
	module.exports = diff
	
	function diff(a, b) {
	    var patch = { a: a }
	    walk(a, b, patch, 0)
	    return patch
	}
	
	function walk(a, b, patch, index) {
	    if (a === b) {
	        return
	    }
	
	    var apply = patch[index]
	    var applyClear = false
	
	    if (isThunk(a) || isThunk(b)) {
	        thunks(a, b, patch, index)
	    } else if (b == null) {
	
	        // If a is a widget we will add a remove patch for it
	        // Otherwise any child widgets/hooks must be destroyed.
	        // This prevents adding two remove patches for a widget.
	        if (!isWidget(a)) {
	            clearState(a, patch, index)
	            apply = patch[index]
	        }
	
	        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b))
	    } else if (isVNode(b)) {
	        if (isVNode(a)) {
	            if (a.tagName === b.tagName &&
	                a.namespace === b.namespace &&
	                a.key === b.key) {
	                var propsPatch = diffProps(a.properties, b.properties)
	                if (propsPatch) {
	                    apply = appendPatch(apply,
	                        new VPatch(VPatch.PROPS, a, propsPatch))
	                }
	                apply = diffChildren(a, b, patch, apply, index)
	            } else {
	                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
	                applyClear = true
	            }
	        } else {
	            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
	            applyClear = true
	        }
	    } else if (isVText(b)) {
	        if (!isVText(a)) {
	            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
	            applyClear = true
	        } else if (a.text !== b.text) {
	            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
	        }
	    } else if (isWidget(b)) {
	        if (!isWidget(a)) {
	            applyClear = true
	        }
	
	        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b))
	    }
	
	    if (apply) {
	        patch[index] = apply
	    }
	
	    if (applyClear) {
	        clearState(a, patch, index)
	    }
	}
	
	function diffChildren(a, b, patch, apply, index) {
	    var aChildren = a.children
	    var orderedSet = reorder(aChildren, b.children)
	    var bChildren = orderedSet.children
	
	    var aLen = aChildren.length
	    var bLen = bChildren.length
	    var len = aLen > bLen ? aLen : bLen
	
	    for (var i = 0; i < len; i++) {
	        var leftNode = aChildren[i]
	        var rightNode = bChildren[i]
	        index += 1
	
	        if (!leftNode) {
	            if (rightNode) {
	                // Excess nodes in b need to be added
	                apply = appendPatch(apply,
	                    new VPatch(VPatch.INSERT, null, rightNode))
	            }
	        } else {
	            walk(leftNode, rightNode, patch, index)
	        }
	
	        if (isVNode(leftNode) && leftNode.count) {
	            index += leftNode.count
	        }
	    }
	
	    if (orderedSet.moves) {
	        // Reorder nodes last
	        apply = appendPatch(apply, new VPatch(
	            VPatch.ORDER,
	            a,
	            orderedSet.moves
	        ))
	    }
	
	    return apply
	}
	
	function clearState(vNode, patch, index) {
	    // TODO: Make this a single walk, not two
	    unhook(vNode, patch, index)
	    destroyWidgets(vNode, patch, index)
	}
	
	// Patch records for all destroyed widgets must be added because we need
	// a DOM node reference for the destroy function
	function destroyWidgets(vNode, patch, index) {
	    if (isWidget(vNode)) {
	        if (typeof vNode.destroy === "function") {
	            patch[index] = appendPatch(
	                patch[index],
	                new VPatch(VPatch.REMOVE, vNode, null)
	            )
	        }
	    } else if (isVNode(vNode) && (vNode.hasWidgets || vNode.hasThunks)) {
	        var children = vNode.children
	        var len = children.length
	        for (var i = 0; i < len; i++) {
	            var child = children[i]
	            index += 1
	
	            destroyWidgets(child, patch, index)
	
	            if (isVNode(child) && child.count) {
	                index += child.count
	            }
	        }
	    } else if (isThunk(vNode)) {
	        thunks(vNode, null, patch, index)
	    }
	}
	
	// Create a sub-patch for thunks
	function thunks(a, b, patch, index) {
	    var nodes = handleThunk(a, b)
	    var thunkPatch = diff(nodes.a, nodes.b)
	    if (hasPatches(thunkPatch)) {
	        patch[index] = new VPatch(VPatch.THUNK, null, thunkPatch)
	    }
	}
	
	function hasPatches(patch) {
	    for (var index in patch) {
	        if (index !== "a") {
	            return true
	        }
	    }
	
	    return false
	}
	
	// Execute hooks when two nodes are identical
	function unhook(vNode, patch, index) {
	    if (isVNode(vNode)) {
	        if (vNode.hooks) {
	            patch[index] = appendPatch(
	                patch[index],
	                new VPatch(
	                    VPatch.PROPS,
	                    vNode,
	                    undefinedKeys(vNode.hooks)
	                )
	            )
	        }
	
	        if (vNode.descendantHooks || vNode.hasThunks) {
	            var children = vNode.children
	            var len = children.length
	            for (var i = 0; i < len; i++) {
	                var child = children[i]
	                index += 1
	
	                unhook(child, patch, index)
	
	                if (isVNode(child) && child.count) {
	                    index += child.count
	                }
	            }
	        }
	    } else if (isThunk(vNode)) {
	        thunks(vNode, null, patch, index)
	    }
	}
	
	function undefinedKeys(obj) {
	    var result = {}
	
	    for (var key in obj) {
	        result[key] = undefined
	    }
	
	    return result
	}
	
	// List diff, naive left to right reordering
	function reorder(aChildren, bChildren) {
	    // O(M) time, O(M) memory
	    var bChildIndex = keyIndex(bChildren)
	    var bKeys = bChildIndex.keys
	    var bFree = bChildIndex.free
	
	    if (bFree.length === bChildren.length) {
	        return {
	            children: bChildren,
	            moves: null
	        }
	    }
	
	    // O(N) time, O(N) memory
	    var aChildIndex = keyIndex(aChildren)
	    var aKeys = aChildIndex.keys
	    var aFree = aChildIndex.free
	
	    if (aFree.length === aChildren.length) {
	        return {
	            children: bChildren,
	            moves: null
	        }
	    }
	
	    // O(MAX(N, M)) memory
	    var newChildren = []
	
	    var freeIndex = 0
	    var freeCount = bFree.length
	    var deletedItems = 0
	
	    // Iterate through a and match a node in b
	    // O(N) time,
	    for (var i = 0 ; i < aChildren.length; i++) {
	        var aItem = aChildren[i]
	        var itemIndex
	
	        if (aItem.key) {
	            if (bKeys.hasOwnProperty(aItem.key)) {
	                // Match up the old keys
	                itemIndex = bKeys[aItem.key]
	                newChildren.push(bChildren[itemIndex])
	
	            } else {
	                // Remove old keyed items
	                itemIndex = i - deletedItems++
	                newChildren.push(null)
	            }
	        } else {
	            // Match the item in a with the next free item in b
	            if (freeIndex < freeCount) {
	                itemIndex = bFree[freeIndex++]
	                newChildren.push(bChildren[itemIndex])
	            } else {
	                // There are no free items in b to match with
	                // the free items in a, so the extra free nodes
	                // are deleted.
	                itemIndex = i - deletedItems++
	                newChildren.push(null)
	            }
	        }
	    }
	
	    var lastFreeIndex = freeIndex >= bFree.length ?
	        bChildren.length :
	        bFree[freeIndex]
	
	    // Iterate through b and append any new keys
	    // O(M) time
	    for (var j = 0; j < bChildren.length; j++) {
	        var newItem = bChildren[j]
	
	        if (newItem.key) {
	            if (!aKeys.hasOwnProperty(newItem.key)) {
	                // Add any new keyed items
	                // We are adding new items to the end and then sorting them
	                // in place. In future we should insert new items in place.
	                newChildren.push(newItem)
	            }
	        } else if (j >= lastFreeIndex) {
	            // Add any leftover non-keyed items
	            newChildren.push(newItem)
	        }
	    }
	
	    var simulate = newChildren.slice()
	    var simulateIndex = 0
	    var removes = []
	    var inserts = []
	    var simulateItem
	
	    for (var k = 0; k < bChildren.length;) {
	        var wantedItem = bChildren[k]
	        simulateItem = simulate[simulateIndex]
	
	        // remove items
	        while (simulateItem === null && simulate.length) {
	            removes.push(remove(simulate, simulateIndex, null))
	            simulateItem = simulate[simulateIndex]
	        }
	
	        if (!simulateItem || simulateItem.key !== wantedItem.key) {
	            // if we need a key in this position...
	            if (wantedItem.key) {
	                if (simulateItem && simulateItem.key) {
	                    // if an insert doesn't put this key in place, it needs to move
	                    if (bKeys[simulateItem.key] !== k + 1) {
	                        removes.push(remove(simulate, simulateIndex, simulateItem.key))
	                        simulateItem = simulate[simulateIndex]
	                        // if the remove didn't put the wanted item in place, we need to insert it
	                        if (!simulateItem || simulateItem.key !== wantedItem.key) {
	                            inserts.push({key: wantedItem.key, to: k})
	                        }
	                        // items are matching, so skip ahead
	                        else {
	                            simulateIndex++
	                        }
	                    }
	                    else {
	                        inserts.push({key: wantedItem.key, to: k})
	                    }
	                }
	                else {
	                    inserts.push({key: wantedItem.key, to: k})
	                }
	                k++
	            }
	            // a key in simulate has no matching wanted key, remove it
	            else if (simulateItem && simulateItem.key) {
	                removes.push(remove(simulate, simulateIndex, simulateItem.key))
	            }
	        }
	        else {
	            simulateIndex++
	            k++
	        }
	    }
	
	    // remove all the remaining nodes from simulate
	    while(simulateIndex < simulate.length) {
	        simulateItem = simulate[simulateIndex]
	        removes.push(remove(simulate, simulateIndex, simulateItem && simulateItem.key))
	    }
	
	    // If the only moves we have are deletes then we can just
	    // let the delete patch remove these items.
	    if (removes.length === deletedItems && !inserts.length) {
	        return {
	            children: newChildren,
	            moves: null
	        }
	    }
	
	    return {
	        children: newChildren,
	        moves: {
	            removes: removes,
	            inserts: inserts
	        }
	    }
	}
	
	function remove(arr, index, key) {
	    arr.splice(index, 1)
	
	    return {
	        from: index,
	        key: key
	    }
	}
	
	function keyIndex(children) {
	    var keys = {}
	    var free = []
	    var length = children.length
	
	    for (var i = 0; i < length; i++) {
	        var child = children[i]
	
	        if (child.key) {
	            keys[child.key] = i
	        } else {
	            free.push(i)
	        }
	    }
	
	    return {
	        keys: keys,     // A hash of key name to index
	        free: free      // An array of unkeyed item indices
	    }
	}
	
	function appendPatch(apply, patch) {
	    if (apply) {
	        if (isArray(apply)) {
	            apply.push(patch)
	        } else {
	            apply = [apply, patch]
	        }
	
	        return apply
	    } else {
	        return patch
	    }
	}


/***/ },
/* 6 */
/***/ function(module, exports) {

	var nativeIsArray = Array.isArray
	var toString = Object.prototype.toString
	
	module.exports = nativeIsArray || isArray
	
	function isArray(obj) {
	    return toString.call(obj) === "[object Array]"
	}


/***/ },
/* 7 */
/***/ function(module, exports, __webpack_require__) {

	var version = __webpack_require__(8)
	
	VirtualPatch.NONE = 0
	VirtualPatch.VTEXT = 1
	VirtualPatch.VNODE = 2
	VirtualPatch.WIDGET = 3
	VirtualPatch.PROPS = 4
	VirtualPatch.ORDER = 5
	VirtualPatch.INSERT = 6
	VirtualPatch.REMOVE = 7
	VirtualPatch.THUNK = 8
	
	module.exports = VirtualPatch
	
	function VirtualPatch(type, vNode, patch) {
	    this.type = Number(type)
	    this.vNode = vNode
	    this.patch = patch
	}
	
	VirtualPatch.prototype.version = version
	VirtualPatch.prototype.type = "VirtualPatch"


/***/ },
/* 8 */
/***/ function(module, exports) {

	module.exports = "2"


/***/ },
/* 9 */
/***/ function(module, exports, __webpack_require__) {

	var version = __webpack_require__(8)
	
	module.exports = isVirtualNode
	
	function isVirtualNode(x) {
	    return x && x.type === "VirtualNode" && x.version === version
	}


/***/ },
/* 10 */
/***/ function(module, exports, __webpack_require__) {

	var version = __webpack_require__(8)
	
	module.exports = isVirtualText
	
	function isVirtualText(x) {
	    return x && x.type === "VirtualText" && x.version === version
	}


/***/ },
/* 11 */
/***/ function(module, exports) {

	module.exports = isWidget
	
	function isWidget(w) {
	    return w && w.type === "Widget"
	}


/***/ },
/* 12 */
/***/ function(module, exports) {

	module.exports = isThunk
	
	function isThunk(t) {
	    return t && t.type === "Thunk"
	}


/***/ },
/* 13 */
/***/ function(module, exports, __webpack_require__) {

	var isVNode = __webpack_require__(9)
	var isVText = __webpack_require__(10)
	var isWidget = __webpack_require__(11)
	var isThunk = __webpack_require__(12)
	
	module.exports = handleThunk
	
	function handleThunk(a, b) {
	    var renderedA = a
	    var renderedB = b
	
	    if (isThunk(b)) {
	        renderedB = renderThunk(b, a)
	    }
	
	    if (isThunk(a)) {
	        renderedA = renderThunk(a, null)
	    }
	
	    return {
	        a: renderedA,
	        b: renderedB
	    }
	}
	
	function renderThunk(thunk, previous) {
	    var renderedThunk = thunk.vnode
	
	    if (!renderedThunk) {
	        renderedThunk = thunk.vnode = thunk.render(previous)
	    }
	
	    if (!(isVNode(renderedThunk) ||
	            isVText(renderedThunk) ||
	            isWidget(renderedThunk))) {
	        throw new Error("thunk did not return a valid node");
	    }
	
	    return renderedThunk
	}


/***/ },
/* 14 */
/***/ function(module, exports, __webpack_require__) {

	var isObject = __webpack_require__(15)
	var isHook = __webpack_require__(16)
	
	module.exports = diffProps
	
	function diffProps(a, b) {
	    var diff
	
	    for (var aKey in a) {
	        if (!(aKey in b)) {
	            diff = diff || {}
	            diff[aKey] = undefined
	        }
	
	        var aValue = a[aKey]
	        var bValue = b[aKey]
	
	        if (aValue === bValue) {
	            continue
	        } else if (isObject(aValue) && isObject(bValue)) {
	            if (getPrototype(bValue) !== getPrototype(aValue)) {
	                diff = diff || {}
	                diff[aKey] = bValue
	            } else if (isHook(bValue)) {
	                 diff = diff || {}
	                 diff[aKey] = bValue
	            } else {
	                var objectDiff = diffProps(aValue, bValue)
	                if (objectDiff) {
	                    diff = diff || {}
	                    diff[aKey] = objectDiff
	                }
	            }
	        } else {
	            diff = diff || {}
	            diff[aKey] = bValue
	        }
	    }
	
	    for (var bKey in b) {
	        if (!(bKey in a)) {
	            diff = diff || {}
	            diff[bKey] = b[bKey]
	        }
	    }
	
	    return diff
	}
	
	function getPrototype(value) {
	  if (Object.getPrototypeOf) {
	    return Object.getPrototypeOf(value)
	  } else if (value.__proto__) {
	    return value.__proto__
	  } else if (value.constructor) {
	    return value.constructor.prototype
	  }
	}


/***/ },
/* 15 */
/***/ function(module, exports) {

	"use strict";
	
	module.exports = function isObject(x) {
		return typeof x === "object" && x !== null;
	};


/***/ },
/* 16 */
/***/ function(module, exports) {

	module.exports = isHook
	
	function isHook(hook) {
	    return hook &&
	      (typeof hook.hook === "function" && !hook.hasOwnProperty("hook") ||
	       typeof hook.unhook === "function" && !hook.hasOwnProperty("unhook"))
	}


/***/ },
/* 17 */
/***/ function(module, exports, __webpack_require__) {

	var patch = __webpack_require__(18)
	
	module.exports = patch


/***/ },
/* 18 */
/***/ function(module, exports, __webpack_require__) {

	var document = __webpack_require__(19)
	var isArray = __webpack_require__(6)
	
	var render = __webpack_require__(21)
	var domIndex = __webpack_require__(23)
	var patchOp = __webpack_require__(24)
	module.exports = patch
	
	function patch(rootNode, patches, renderOptions) {
	    renderOptions = renderOptions || {}
	    renderOptions.patch = renderOptions.patch && renderOptions.patch !== patch
	        ? renderOptions.patch
	        : patchRecursive
	    renderOptions.render = renderOptions.render || render
	
	    return renderOptions.patch(rootNode, patches, renderOptions)
	}
	
	function patchRecursive(rootNode, patches, renderOptions) {
	    var indices = patchIndices(patches)
	
	    if (indices.length === 0) {
	        return rootNode
	    }
	
	    var index = domIndex(rootNode, patches.a, indices)
	    var ownerDocument = rootNode.ownerDocument
	
	    if (!renderOptions.document && ownerDocument !== document) {
	        renderOptions.document = ownerDocument
	    }
	
	    for (var i = 0; i < indices.length; i++) {
	        var nodeIndex = indices[i]
	        rootNode = applyPatch(rootNode,
	            index[nodeIndex],
	            patches[nodeIndex],
	            renderOptions)
	    }
	
	    return rootNode
	}
	
	function applyPatch(rootNode, domNode, patchList, renderOptions) {
	    if (!domNode) {
	        return rootNode
	    }
	
	    var newNode
	
	    if (isArray(patchList)) {
	        for (var i = 0; i < patchList.length; i++) {
	            newNode = patchOp(patchList[i], domNode, renderOptions)
	
	            if (domNode === rootNode) {
	                rootNode = newNode
	            }
	        }
	    } else {
	        newNode = patchOp(patchList, domNode, renderOptions)
	
	        if (domNode === rootNode) {
	            rootNode = newNode
	        }
	    }
	
	    return rootNode
	}
	
	function patchIndices(patches) {
	    var indices = []
	
	    for (var key in patches) {
	        if (key !== "a") {
	            indices.push(Number(key))
	        }
	    }
	
	    return indices
	}


/***/ },
/* 19 */
/***/ function(module, exports, __webpack_require__) {

	/* WEBPACK VAR INJECTION */(function(global) {var topLevel = typeof global !== 'undefined' ? global :
	    typeof window !== 'undefined' ? window : {}
	var minDoc = __webpack_require__(20);
	
	if (typeof document !== 'undefined') {
	    module.exports = document;
	} else {
	    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];
	
	    if (!doccy) {
	        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
	    }
	
	    module.exports = doccy;
	}
	
	/* WEBPACK VAR INJECTION */}.call(exports, (function() { return this; }())))

/***/ },
/* 20 */
/***/ function(module, exports) {

	/* (ignored) */

/***/ },
/* 21 */
/***/ function(module, exports, __webpack_require__) {

	var document = __webpack_require__(19)
	
	var applyProperties = __webpack_require__(22)
	
	var isVNode = __webpack_require__(9)
	var isVText = __webpack_require__(10)
	var isWidget = __webpack_require__(11)
	var handleThunk = __webpack_require__(13)
	
	module.exports = createElement
	
	function createElement(vnode, opts) {
	    var doc = opts ? opts.document || document : document
	    var warn = opts ? opts.warn : null
	
	    vnode = handleThunk(vnode).a
	
	    if (isWidget(vnode)) {
	        return vnode.init()
	    } else if (isVText(vnode)) {
	        return doc.createTextNode(vnode.text)
	    } else if (!isVNode(vnode)) {
	        if (warn) {
	            warn("Item is not a valid virtual dom node", vnode)
	        }
	        return null
	    }
	
	    var node = (vnode.namespace === null) ?
	        doc.createElement(vnode.tagName) :
	        doc.createElementNS(vnode.namespace, vnode.tagName)
	
	    var props = vnode.properties
	    applyProperties(node, props)
	
	    var children = vnode.children
	
	    for (var i = 0; i < children.length; i++) {
	        var childNode = createElement(children[i], opts)
	        if (childNode) {
	            node.appendChild(childNode)
	        }
	    }
	
	    return node
	}


/***/ },
/* 22 */
/***/ function(module, exports, __webpack_require__) {

	var isObject = __webpack_require__(15)
	var isHook = __webpack_require__(16)
	
	module.exports = applyProperties
	
	function applyProperties(node, props, previous) {
	    for (var propName in props) {
	        var propValue = props[propName]
	
	        if (propValue === undefined) {
	            removeProperty(node, propName, propValue, previous);
	        } else if (isHook(propValue)) {
	            removeProperty(node, propName, propValue, previous)
	            if (propValue.hook) {
	                propValue.hook(node,
	                    propName,
	                    previous ? previous[propName] : undefined)
	            }
	        } else {
	            if (isObject(propValue)) {
	                patchObject(node, props, previous, propName, propValue);
	            } else {
	                node[propName] = propValue
	            }
	        }
	    }
	}
	
	function removeProperty(node, propName, propValue, previous) {
	    if (previous) {
	        var previousValue = previous[propName]
	
	        if (!isHook(previousValue)) {
	            if (propName === "attributes") {
	                for (var attrName in previousValue) {
	                    node.removeAttribute(attrName)
	                }
	            } else if (propName === "style") {
	                for (var i in previousValue) {
	                    node.style[i] = ""
	                }
	            } else if (typeof previousValue === "string") {
	                node[propName] = ""
	            } else {
	                node[propName] = null
	            }
	        } else if (previousValue.unhook) {
	            previousValue.unhook(node, propName, propValue)
	        }
	    }
	}
	
	function patchObject(node, props, previous, propName, propValue) {
	    var previousValue = previous ? previous[propName] : undefined
	
	    // Set attributes
	    if (propName === "attributes") {
	        for (var attrName in propValue) {
	            var attrValue = propValue[attrName]
	
	            if (attrValue === undefined) {
	                node.removeAttribute(attrName)
	            } else {
	                node.setAttribute(attrName, attrValue)
	            }
	        }
	
	        return
	    }
	
	    if(previousValue && isObject(previousValue) &&
	        getPrototype(previousValue) !== getPrototype(propValue)) {
	        node[propName] = propValue
	        return
	    }
	
	    if (!isObject(node[propName])) {
	        node[propName] = {}
	    }
	
	    var replacer = propName === "style" ? "" : undefined
	
	    for (var k in propValue) {
	        var value = propValue[k]
	        node[propName][k] = (value === undefined) ? replacer : value
	    }
	}
	
	function getPrototype(value) {
	    if (Object.getPrototypeOf) {
	        return Object.getPrototypeOf(value)
	    } else if (value.__proto__) {
	        return value.__proto__
	    } else if (value.constructor) {
	        return value.constructor.prototype
	    }
	}


/***/ },
/* 23 */
/***/ function(module, exports) {

	// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
	// We don't want to read all of the DOM nodes in the tree so we use
	// the in-order tree indexing to eliminate recursion down certain branches.
	// We only recurse into a DOM node if we know that it contains a child of
	// interest.
	
	var noChild = {}
	
	module.exports = domIndex
	
	function domIndex(rootNode, tree, indices, nodes) {
	    if (!indices || indices.length === 0) {
	        return {}
	    } else {
	        indices.sort(ascending)
	        return recurse(rootNode, tree, indices, nodes, 0)
	    }
	}
	
	function recurse(rootNode, tree, indices, nodes, rootIndex) {
	    nodes = nodes || {}
	
	
	    if (rootNode) {
	        if (indexInRange(indices, rootIndex, rootIndex)) {
	            nodes[rootIndex] = rootNode
	        }
	
	        var vChildren = tree.children
	
	        if (vChildren) {
	
	            var childNodes = rootNode.childNodes
	
	            for (var i = 0; i < tree.children.length; i++) {
	                rootIndex += 1
	
	                var vChild = vChildren[i] || noChild
	                var nextIndex = rootIndex + (vChild.count || 0)
	
	                // skip recursion down the tree if there are no nodes down here
	                if (indexInRange(indices, rootIndex, nextIndex)) {
	                    recurse(childNodes[i], vChild, indices, nodes, rootIndex)
	                }
	
	                rootIndex = nextIndex
	            }
	        }
	    }
	
	    return nodes
	}
	
	// Binary search for an index in the interval [left, right]
	function indexInRange(indices, left, right) {
	    if (indices.length === 0) {
	        return false
	    }
	
	    var minIndex = 0
	    var maxIndex = indices.length - 1
	    var currentIndex
	    var currentItem
	
	    while (minIndex <= maxIndex) {
	        currentIndex = ((maxIndex + minIndex) / 2) >> 0
	        currentItem = indices[currentIndex]
	
	        if (minIndex === maxIndex) {
	            return currentItem >= left && currentItem <= right
	        } else if (currentItem < left) {
	            minIndex = currentIndex + 1
	        } else  if (currentItem > right) {
	            maxIndex = currentIndex - 1
	        } else {
	            return true
	        }
	    }
	
	    return false;
	}
	
	function ascending(a, b) {
	    return a > b ? 1 : -1
	}


/***/ },
/* 24 */
/***/ function(module, exports, __webpack_require__) {

	var applyProperties = __webpack_require__(22)
	
	var isWidget = __webpack_require__(11)
	var VPatch = __webpack_require__(7)
	
	var updateWidget = __webpack_require__(25)
	
	module.exports = applyPatch
	
	function applyPatch(vpatch, domNode, renderOptions) {
	    var type = vpatch.type
	    var vNode = vpatch.vNode
	    var patch = vpatch.patch
	
	    switch (type) {
	        case VPatch.REMOVE:
	            return removeNode(domNode, vNode)
	        case VPatch.INSERT:
	            return insertNode(domNode, patch, renderOptions)
	        case VPatch.VTEXT:
	            return stringPatch(domNode, vNode, patch, renderOptions)
	        case VPatch.WIDGET:
	            return widgetPatch(domNode, vNode, patch, renderOptions)
	        case VPatch.VNODE:
	            return vNodePatch(domNode, vNode, patch, renderOptions)
	        case VPatch.ORDER:
	            reorderChildren(domNode, patch)
	            return domNode
	        case VPatch.PROPS:
	            applyProperties(domNode, patch, vNode.properties)
	            return domNode
	        case VPatch.THUNK:
	            return replaceRoot(domNode,
	                renderOptions.patch(domNode, patch, renderOptions))
	        default:
	            return domNode
	    }
	}
	
	function removeNode(domNode, vNode) {
	    var parentNode = domNode.parentNode
	
	    if (parentNode) {
	        parentNode.removeChild(domNode)
	    }
	
	    destroyWidget(domNode, vNode);
	
	    return null
	}
	
	function insertNode(parentNode, vNode, renderOptions) {
	    var newNode = renderOptions.render(vNode, renderOptions)
	
	    if (parentNode) {
	        parentNode.appendChild(newNode)
	    }
	
	    return parentNode
	}
	
	function stringPatch(domNode, leftVNode, vText, renderOptions) {
	    var newNode
	
	    if (domNode.nodeType === 3) {
	        domNode.replaceData(0, domNode.length, vText.text)
	        newNode = domNode
	    } else {
	        var parentNode = domNode.parentNode
	        newNode = renderOptions.render(vText, renderOptions)
	
	        if (parentNode && newNode !== domNode) {
	            parentNode.replaceChild(newNode, domNode)
	        }
	    }
	
	    return newNode
	}
	
	function widgetPatch(domNode, leftVNode, widget, renderOptions) {
	    var updating = updateWidget(leftVNode, widget)
	    var newNode
	
	    if (updating) {
	        newNode = widget.update(leftVNode, domNode) || domNode
	    } else {
	        newNode = renderOptions.render(widget, renderOptions)
	    }
	
	    var parentNode = domNode.parentNode
	
	    if (parentNode && newNode !== domNode) {
	        parentNode.replaceChild(newNode, domNode)
	    }
	
	    if (!updating) {
	        destroyWidget(domNode, leftVNode)
	    }
	
	    return newNode
	}
	
	function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
	    var parentNode = domNode.parentNode
	    var newNode = renderOptions.render(vNode, renderOptions)
	
	    if (parentNode && newNode !== domNode) {
	        parentNode.replaceChild(newNode, domNode)
	    }
	
	    return newNode
	}
	
	function destroyWidget(domNode, w) {
	    if (typeof w.destroy === "function" && isWidget(w)) {
	        w.destroy(domNode)
	    }
	}
	
	function reorderChildren(domNode, moves) {
	    var childNodes = domNode.childNodes
	    var keyMap = {}
	    var node
	    var remove
	    var insert
	
	    for (var i = 0; i < moves.removes.length; i++) {
	        remove = moves.removes[i]
	        node = childNodes[remove.from]
	        if (remove.key) {
	            keyMap[remove.key] = node
	        }
	        domNode.removeChild(node)
	    }
	
	    var length = childNodes.length
	    for (var j = 0; j < moves.inserts.length; j++) {
	        insert = moves.inserts[j]
	        node = keyMap[insert.key]
	        // this is the weirdest bug i've ever seen in webkit
	        domNode.insertBefore(node, insert.to >= length++ ? null : childNodes[insert.to])
	    }
	}
	
	function replaceRoot(oldRoot, newRoot) {
	    if (oldRoot && newRoot && oldRoot !== newRoot && oldRoot.parentNode) {
	        oldRoot.parentNode.replaceChild(newRoot, oldRoot)
	    }
	
	    return newRoot;
	}


/***/ },
/* 25 */
/***/ function(module, exports, __webpack_require__) {

	var isWidget = __webpack_require__(11)
	
	module.exports = updateWidget
	
	function updateWidget(a, b) {
	    if (isWidget(a) && isWidget(b)) {
	        if ("name" in a && "name" in b) {
	            return a.id === b.id
	        } else {
	            return a.init === b.init
	        }
	    }
	
	    return false
	}


/***/ },
/* 26 */
/***/ function(module, exports, __webpack_require__) {

	var h = __webpack_require__(27)
	
	module.exports = h


/***/ },
/* 27 */
/***/ function(module, exports, __webpack_require__) {

	'use strict';
	
	var isArray = __webpack_require__(6);
	
	var VNode = __webpack_require__(28);
	var VText = __webpack_require__(29);
	var isVNode = __webpack_require__(9);
	var isVText = __webpack_require__(10);
	var isWidget = __webpack_require__(11);
	var isHook = __webpack_require__(16);
	var isVThunk = __webpack_require__(12);
	
	var parseTag = __webpack_require__(30);
	var softSetHook = __webpack_require__(32);
	var evHook = __webpack_require__(33);
	
	module.exports = h;
	
	function h(tagName, properties, children) {
	    var childNodes = [];
	    var tag, props, key, namespace;
	
	    if (!children && isChildren(properties)) {
	        children = properties;
	        props = {};
	    }
	
	    props = props || properties || {};
	    tag = parseTag(tagName, props);
	
	    // support keys
	    if (props.hasOwnProperty('key')) {
	        key = props.key;
	        props.key = undefined;
	    }
	
	    // support namespace
	    if (props.hasOwnProperty('namespace')) {
	        namespace = props.namespace;
	        props.namespace = undefined;
	    }
	
	    // fix cursor bug
	    if (tag === 'INPUT' &&
	        !namespace &&
	        props.hasOwnProperty('value') &&
	        props.value !== undefined &&
	        !isHook(props.value)
	    ) {
	        props.value = softSetHook(props.value);
	    }
	
	    transformProperties(props);
	
	    if (children !== undefined && children !== null) {
	        addChild(children, childNodes, tag, props);
	    }
	
	
	    return new VNode(tag, props, childNodes, key, namespace);
	}
	
	function addChild(c, childNodes, tag, props) {
	    if (typeof c === 'string') {
	        childNodes.push(new VText(c));
	    } else if (typeof c === 'number') {
	        childNodes.push(new VText(String(c)));
	    } else if (isChild(c)) {
	        childNodes.push(c);
	    } else if (isArray(c)) {
	        for (var i = 0; i < c.length; i++) {
	            addChild(c[i], childNodes, tag, props);
	        }
	    } else if (c === null || c === undefined) {
	        return;
	    } else {
	        throw UnexpectedVirtualElement({
	            foreignObject: c,
	            parentVnode: {
	                tagName: tag,
	                properties: props
	            }
	        });
	    }
	}
	
	function transformProperties(props) {
	    for (var propName in props) {
	        if (props.hasOwnProperty(propName)) {
	            var value = props[propName];
	
	            if (isHook(value)) {
	                continue;
	            }
	
	            if (propName.substr(0, 3) === 'ev-') {
	                // add ev-foo support
	                props[propName] = evHook(value);
	            }
	        }
	    }
	}
	
	function isChild(x) {
	    return isVNode(x) || isVText(x) || isWidget(x) || isVThunk(x);
	}
	
	function isChildren(x) {
	    return typeof x === 'string' || isArray(x) || isChild(x);
	}
	
	function UnexpectedVirtualElement(data) {
	    var err = new Error();
	
	    err.type = 'virtual-hyperscript.unexpected.virtual-element';
	    err.message = 'Unexpected virtual child passed to h().\n' +
	        'Expected a VNode / Vthunk / VWidget / string but:\n' +
	        'got:\n' +
	        errorString(data.foreignObject) +
	        '.\n' +
	        'The parent vnode is:\n' +
	        errorString(data.parentVnode)
	        '\n' +
	        'Suggested fix: change your `h(..., [ ... ])` callsite.';
	    err.foreignObject = data.foreignObject;
	    err.parentVnode = data.parentVnode;
	
	    return err;
	}
	
	function errorString(obj) {
	    try {
	        return JSON.stringify(obj, null, '    ');
	    } catch (e) {
	        return String(obj);
	    }
	}


/***/ },
/* 28 */
/***/ function(module, exports, __webpack_require__) {

	var version = __webpack_require__(8)
	var isVNode = __webpack_require__(9)
	var isWidget = __webpack_require__(11)
	var isThunk = __webpack_require__(12)
	var isVHook = __webpack_require__(16)
	
	module.exports = VirtualNode
	
	var noProperties = {}
	var noChildren = []
	
	function VirtualNode(tagName, properties, children, key, namespace) {
	    this.tagName = tagName
	    this.properties = properties || noProperties
	    this.children = children || noChildren
	    this.key = key != null ? String(key) : undefined
	    this.namespace = (typeof namespace === "string") ? namespace : null
	
	    var count = (children && children.length) || 0
	    var descendants = 0
	    var hasWidgets = false
	    var hasThunks = false
	    var descendantHooks = false
	    var hooks
	
	    for (var propName in properties) {
	        if (properties.hasOwnProperty(propName)) {
	            var property = properties[propName]
	            if (isVHook(property) && property.unhook) {
	                if (!hooks) {
	                    hooks = {}
	                }
	
	                hooks[propName] = property
	            }
	        }
	    }
	
	    for (var i = 0; i < count; i++) {
	        var child = children[i]
	        if (isVNode(child)) {
	            descendants += child.count || 0
	
	            if (!hasWidgets && child.hasWidgets) {
	                hasWidgets = true
	            }
	
	            if (!hasThunks && child.hasThunks) {
	                hasThunks = true
	            }
	
	            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
	                descendantHooks = true
	            }
	        } else if (!hasWidgets && isWidget(child)) {
	            if (typeof child.destroy === "function") {
	                hasWidgets = true
	            }
	        } else if (!hasThunks && isThunk(child)) {
	            hasThunks = true;
	        }
	    }
	
	    this.count = count + descendants
	    this.hasWidgets = hasWidgets
	    this.hasThunks = hasThunks
	    this.hooks = hooks
	    this.descendantHooks = descendantHooks
	}
	
	VirtualNode.prototype.version = version
	VirtualNode.prototype.type = "VirtualNode"


/***/ },
/* 29 */
/***/ function(module, exports, __webpack_require__) {

	var version = __webpack_require__(8)
	
	module.exports = VirtualText
	
	function VirtualText(text) {
	    this.text = String(text)
	}
	
	VirtualText.prototype.version = version
	VirtualText.prototype.type = "VirtualText"


/***/ },
/* 30 */
/***/ function(module, exports, __webpack_require__) {

	'use strict';
	
	var split = __webpack_require__(31);
	
	var classIdSplit = /([\.#]?[a-zA-Z0-9\u007F-\uFFFF_:-]+)/;
	var notClassId = /^\.|#/;
	
	module.exports = parseTag;
	
	function parseTag(tag, props) {
	    if (!tag) {
	        return 'DIV';
	    }
	
	    var noId = !(props.hasOwnProperty('id'));
	
	    var tagParts = split(tag, classIdSplit);
	    var tagName = null;
	
	    if (notClassId.test(tagParts[1])) {
	        tagName = 'DIV';
	    }
	
	    var classes, part, type, i;
	
	    for (i = 0; i < tagParts.length; i++) {
	        part = tagParts[i];
	
	        if (!part) {
	            continue;
	        }
	
	        type = part.charAt(0);
	
	        if (!tagName) {
	            tagName = part;
	        } else if (type === '.') {
	            classes = classes || [];
	            classes.push(part.substring(1, part.length));
	        } else if (type === '#' && noId) {
	            props.id = part.substring(1, part.length);
	        }
	    }
	
	    if (classes) {
	        if (props.className) {
	            classes.push(props.className);
	        }
	
	        props.className = classes.join(' ');
	    }
	
	    return props.namespace ? tagName : tagName.toUpperCase();
	}


/***/ },
/* 31 */
/***/ function(module, exports) {

	/*!
	 * Cross-Browser Split 1.1.1
	 * Copyright 2007-2012 Steven Levithan <stevenlevithan.com>
	 * Available under the MIT License
	 * ECMAScript compliant, uniform cross-browser split method
	 */
	
	/**
	 * Splits a string into an array of strings using a regex or string separator. Matches of the
	 * separator are not included in the result array. However, if `separator` is a regex that contains
	 * capturing groups, backreferences are spliced into the result each time `separator` is matched.
	 * Fixes browser bugs compared to the native `String.prototype.split` and can be used reliably
	 * cross-browser.
	 * @param {String} str String to split.
	 * @param {RegExp|String} separator Regex or string to use for separating the string.
	 * @param {Number} [limit] Maximum number of items to include in the result array.
	 * @returns {Array} Array of substrings.
	 * @example
	 *
	 * // Basic use
	 * split('a b c d', ' ');
	 * // -> ['a', 'b', 'c', 'd']
	 *
	 * // With limit
	 * split('a b c d', ' ', 2);
	 * // -> ['a', 'b']
	 *
	 * // Backreferences in result array
	 * split('..word1 word2..', /([a-z]+)(\d+)/i);
	 * // -> ['..', 'word', '1', ' ', 'word', '2', '..']
	 */
	module.exports = (function split(undef) {
	
	  var nativeSplit = String.prototype.split,
	    compliantExecNpcg = /()??/.exec("")[1] === undef,
	    // NPCG: nonparticipating capturing group
	    self;
	
	  self = function(str, separator, limit) {
	    // If `separator` is not a regex, use `nativeSplit`
	    if (Object.prototype.toString.call(separator) !== "[object RegExp]") {
	      return nativeSplit.call(str, separator, limit);
	    }
	    var output = [],
	      flags = (separator.ignoreCase ? "i" : "") + (separator.multiline ? "m" : "") + (separator.extended ? "x" : "") + // Proposed for ES6
	      (separator.sticky ? "y" : ""),
	      // Firefox 3+
	      lastLastIndex = 0,
	      // Make `global` and avoid `lastIndex` issues by working with a copy
	      separator = new RegExp(separator.source, flags + "g"),
	      separator2, match, lastIndex, lastLength;
	    str += ""; // Type-convert
	    if (!compliantExecNpcg) {
	      // Doesn't need flags gy, but they don't hurt
	      separator2 = new RegExp("^" + separator.source + "$(?!\\s)", flags);
	    }
	    /* Values for `limit`, per the spec:
	     * If undefined: 4294967295 // Math.pow(2, 32) - 1
	     * If 0, Infinity, or NaN: 0
	     * If positive number: limit = Math.floor(limit); if (limit > 4294967295) limit -= 4294967296;
	     * If negative number: 4294967296 - Math.floor(Math.abs(limit))
	     * If other: Type-convert, then use the above rules
	     */
	    limit = limit === undef ? -1 >>> 0 : // Math.pow(2, 32) - 1
	    limit >>> 0; // ToUint32(limit)
	    while (match = separator.exec(str)) {
	      // `separator.lastIndex` is not reliable cross-browser
	      lastIndex = match.index + match[0].length;
	      if (lastIndex > lastLastIndex) {
	        output.push(str.slice(lastLastIndex, match.index));
	        // Fix browsers whose `exec` methods don't consistently return `undefined` for
	        // nonparticipating capturing groups
	        if (!compliantExecNpcg && match.length > 1) {
	          match[0].replace(separator2, function() {
	            for (var i = 1; i < arguments.length - 2; i++) {
	              if (arguments[i] === undef) {
	                match[i] = undef;
	              }
	            }
	          });
	        }
	        if (match.length > 1 && match.index < str.length) {
	          Array.prototype.push.apply(output, match.slice(1));
	        }
	        lastLength = match[0].length;
	        lastLastIndex = lastIndex;
	        if (output.length >= limit) {
	          break;
	        }
	      }
	      if (separator.lastIndex === match.index) {
	        separator.lastIndex++; // Avoid an infinite loop
	      }
	    }
	    if (lastLastIndex === str.length) {
	      if (lastLength || !separator.test("")) {
	        output.push("");
	      }
	    } else {
	      output.push(str.slice(lastLastIndex));
	    }
	    return output.length > limit ? output.slice(0, limit) : output;
	  };
	
	  return self;
	})();


/***/ },
/* 32 */
/***/ function(module, exports) {

	'use strict';
	
	module.exports = SoftSetHook;
	
	function SoftSetHook(value) {
	    if (!(this instanceof SoftSetHook)) {
	        return new SoftSetHook(value);
	    }
	
	    this.value = value;
	}
	
	SoftSetHook.prototype.hook = function (node, propertyName) {
	    if (node[propertyName] !== this.value) {
	        node[propertyName] = this.value;
	    }
	};


/***/ },
/* 33 */
/***/ function(module, exports, __webpack_require__) {

	'use strict';
	
	var EvStore = __webpack_require__(34);
	
	module.exports = EvHook;
	
	function EvHook(value) {
	    if (!(this instanceof EvHook)) {
	        return new EvHook(value);
	    }
	
	    this.value = value;
	}
	
	EvHook.prototype.hook = function (node, propertyName) {
	    var es = EvStore(node);
	    var propName = propertyName.substr(3);
	
	    es[propName] = this.value;
	};
	
	EvHook.prototype.unhook = function(node, propertyName) {
	    var es = EvStore(node);
	    var propName = propertyName.substr(3);
	
	    es[propName] = undefined;
	};


/***/ },
/* 34 */
/***/ function(module, exports, __webpack_require__) {

	'use strict';
	
	var OneVersionConstraint = __webpack_require__(35);
	
	var MY_VERSION = '7';
	OneVersionConstraint('ev-store', MY_VERSION);
	
	var hashKey = '__EV_STORE_KEY@' + MY_VERSION;
	
	module.exports = EvStore;
	
	function EvStore(elem) {
	    var hash = elem[hashKey];
	
	    if (!hash) {
	        hash = elem[hashKey] = {};
	    }
	
	    return hash;
	}


/***/ },
/* 35 */
/***/ function(module, exports, __webpack_require__) {

	'use strict';
	
	var Individual = __webpack_require__(36);
	
	module.exports = OneVersion;
	
	function OneVersion(moduleName, version, defaultValue) {
	    var key = '__INDIVIDUAL_ONE_VERSION_' + moduleName;
	    var enforceKey = key + '_ENFORCE_SINGLETON';
	
	    var versionValue = Individual(enforceKey, version);
	
	    if (versionValue !== version) {
	        throw new Error('Can only have one copy of ' +
	            moduleName + '.\n' +
	            'You already have version ' + versionValue +
	            ' installed.\n' +
	            'This means you cannot install version ' + version);
	    }
	
	    return Individual(key, defaultValue);
	}


/***/ },
/* 36 */
/***/ function(module, exports) {

	/* WEBPACK VAR INJECTION */(function(global) {'use strict';
	
	/*global window, global*/
	
	var root = typeof window !== 'undefined' ?
	    window : typeof global !== 'undefined' ?
	    global : {};
	
	module.exports = Individual;
	
	function Individual(key, value) {
	    if (key in root) {
	        return root[key];
	    }
	
	    root[key] = value;
	
	    return value;
	}
	
	/* WEBPACK VAR INJECTION */}.call(exports, (function() { return this; }())))

/***/ },
/* 37 */
/***/ function(module, exports, __webpack_require__) {

	var createElement = __webpack_require__(21)
	
	module.exports = createElement


/***/ }
/******/ ]);
//# sourceMappingURL=bundle.js.map