/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_CORE_CID = Components.ID("{e4e95e7f-12f1-4b21-8155-82eb22b88c86}");
const NS_CORE_PROG_ID = "@downloadhelper.net/core;1";
const DHNS = "http://downloadhelper.net/1.0#";
const CICORE = Components.interfaces.dhICore;
const CTRL_KEY = 1;
const SHIFT_KEY = 2;
const ALT_KEY = 4;
const META_KEY = 8;

var Util=null;

/**
* Object constructor
*/
function Core() {
	try {
		//dump("[Core] constructor\n");
		this.promptService=Components.classes["@mozilla.org/embedcomp/prompt-service;1"]
		                          			.getService(Components.interfaces.nsIPromptService);
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                                   .getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.");
		this.prefBranch2=this.pref.QueryInterface(Components.interfaces.nsIPrefBranch2);
		this.prefBranch2.addObserver("", this, false);
		this.updateProcessorKeyMap();
		this.observerService =
			Components.classes["@mozilla.org/observer-service;1"]
		    	.getService(Components.interfaces.nsIObserverService);
		this.listMgr=Components.classes["@downloadhelper.net/media-list-manager"]
			                        	.getService(Components.interfaces.dhIMediaListMgr);
		this.dlMgr=Components.classes["@downloadhelper.net/download-manager;1"]
			                        	.getService(Components.interfaces.dhIDownloadMgr);
		this.cvMgr=Components.classes["@downloadhelper.net/convert-manager-component"]
		              					.getService(Components.interfaces.dhIConvertMgr);
		this.smartNamer = Components.classes["@downloadhelper.net/smart-namer;1"]
		                                .getService(Components.interfaces.dhISmartNamer);

		this.regMenus=[];
		this.probes=[];
		this.entries=[];
		this.processors=[];
		this.ctxItems=[];
		this.blacklist=[];
		
		this.updateBlackList();
		this.shareBlackList();

		this.observerService.addObserver(this,"http-on-modify-request",false);
		this.observerService.addObserver(this,"http-on-examine-response",false);
		this.observerService.addObserver(this,"quit-application",false);

		try {
			this.observerService.addObserver(this,"private-browsing",false);
			this.observerService.addObserver(this,"browser:purge-session-history",false);			  
		} catch(e) {}
	
	} catch(e) {
		dump("[Core] !!! constructor: "+e+"\n");
	}
}

Core.prototype = {
}

Core.prototype.registerMenu=function(menupopup,menutype) {
	//dump("[Core] registerMenu("+menupopup+","+menutype+")\n");
	if(menutype==Components.interfaces.dhICore.MENU_TYPE_DOWNLOAD) {
		var button=null;
		var buttonId=menupopup.getAttribute("dh-controlled-button");
		if(buttonId!=null && buttonId.length>0) {
			button=menupopup.ownerDocument.getElementById(buttonId);
			if(button && !button.hasAttribute("dh-installed-handler")) {
				function Listener(core,window,button) {
					this.core=core;
					this.window=window;
					this.button=button;
				}
				Listener.prototype={
					handleEvent: function(event) {
						if(event.target==this.button)
							this.core.buttonClicked(this.window);
					}
				}
				var buttonTarget=button.QueryInterface(Components.interfaces.nsIDOMNSEventTarget);
				buttonTarget.addEventListener("command",new Listener(this,menupopup.ownerDocument.defaultView,button),false,false);
				button.setAttribute("dh-installed-handler","true");
			}
		}
		var hideParentIfEmpty=false;
		if(menupopup.getAttribute("hide-parent-if-empty")=="true")
			hideParentIfEmpty=true;
		this.regMenus.push({
			//menupopup: menupopup.QueryInterface(Components.interfaces.nsISupportsWeakReference).GetWeakReference(),
			menupopup: menupopup,
			menutype:menutype,
			window: menupopup.ownerDocument.defaultView,
			button: button,
			hideParentIfEmpty: hideParentIfEmpty
		});
		try {
			var window=menupopup.ownerDocument.defaultView;
			var document=window.contentDocument;
			this.updateMenus(document,window);
		} catch(e) {}
		//dump("=>"+this.regMenus.length+"\n");
	} else if(menutype==Components.interfaces.dhICore.MENU_TYPE_SYSTEM) {
		menupopup=menupopup.QueryInterface(Components.interfaces.nsIDOMNSEventTarget);
		function Listener(core) {
			this.core=core;
		}
		Listener.prototype={
			handleEvent: function(event) {
				if(event.target.getAttribute("class")=="SystemMenu")
					this.core.updateSystemMenu(event.target);
			}
		}
		menupopup.addEventListener("popupshowing",new Listener(this),false,false);		
	}
}

Core.prototype.unregisterMenu=function(menupopup) {
	//dump("[Core] unregisterMenu("+menupopup+")\n");	
	var found=false;
	for(var i in this.regMenus) {
		if(this.regMenus[i].menupopup==menupopup) {
			this.regMenus.splice(i,1);
			found=true;
			//dump("=>"+this.regMenus.length+"\n");
			break;
		}
	}
	if(!found) {
		menupopup=menupopup.QueryInterface(Components.interfaces.nsIDOMNSEventTarget);
		// must be a system menu, but no way to remove event listener
	}
}

Core.prototype.registerProbe=function(probe) {
	//dump("[Core] registerProbe("+probe+")\n");
	this.probes.push(probe);
}

Core.prototype.unregisterProbe=function(probe) {
	//dump("[Core] unregisterProbe("+probe+")\n");
	for(var i in this.probes) {
		if(this.probes[i]==probe) {
			this.probes.splice(i,1);
			break;
		}
	}
}

Core.prototype.observe=function(subject, topic , data) {
	//dump("[Core] observe("+subject+","+topic+","+data+")\n");	
	try {
	switch(topic) {
		case "http-on-modify-request":
			var channel=subject.QueryInterface(Components.interfaces.nsIHttpChannel);
			if(channel.requestMethod!="GET")
				return;
		    var request=subject.QueryInterface(Components.interfaces.nsIRequest);
			//dump("[Core] observe/http-on-modify-request "+request.name+"\n");
		    if(this.listMgr.checkCurrentURL(request.name)) {
				return;
		    }
			for(var i in this.probes) {
				var probe=this.probes[i];
				if(probe.handleRequest) {
					try {
						probe.handleRequest(request);
					} catch(e) {
					}
				}
			}
		    break;
		    
		case "http-on-examine-response":
		    var request=subject.QueryInterface(Components.interfaces.nsIRequest);
			//dump("[Core] observe/http-on-examine-response "+request.name+"\n");
		    if(this.listMgr.checkCurrentURL(request.name)) {
				try {
				    var channel=subject.QueryInterface(Components.interfaces.nsIHttpChannel);
				    var location=channel.getResponseHeader("Location");
				    if(location!=null) {
				    	this.listMgr.addCurrentURL(location);
				    }
				} catch(e) {}
				return;
		    }
			for(var i in this.probes) {
				var probe=this.probes[i];
				if(probe.handleResponse) {
					try {
						probe.handleResponse(request);
					} catch(e) {
						dump("!!! [Core] observe("+subject+","+topic+","+data+"): "+e+"\n");
					}
				}
			}
		    break;
		    
		case "nsPref:changed":
			if(data=="processor-keymap")
				this.updateProcessorKeyMap();
			if(data=="disable-dwcount-cookie")
				this.removeDownloadCookie();
			if(data=="media-host-blacklist")
				this.updateBlackList();
			break;
			
		case "quit-application":
			//dump("[Core] observe/quit-application\n");
			this.prefBranch2.removeObserver("",this);
			this.observerService.removeObserver(this,"http-on-modify-request");
			this.observerService.removeObserver(this,"http-on-examine-response");
			this.observerService.removeObserver(this,"quit-application");
			break;
			
		case "private-browsing":
			if (data == "enter")
				Util.setUnicharPref(this.pref,"storagedirectory0",Util.getUnicharPref(this.pref,"storagedirectory",null));
			else if(data == "exit") {
				Util.setUnicharPref(this.pref,"storagedirectory",Util.getUnicharPref(this.pref,"storagedirectory0",null));
				this.pref.clearUserPref("storagedirectory0");
			}
			break;

	}
	} catch(e) {
		dump("!!! [Core] observe("+subject+","+topic+","+data+"): "+e+"\n");	
	}
}

Core.prototype.handleEvent=function(event) {
	//dump("[Core] handleEvent("+event.type+"/"+event.eventPhase+"/"+event.target+")\n");
	var window=event.target.defaultView;
	//dump("Window="+window+"\n");
	switch(event.type) {
		case "select":
			//dump("select - "+event.target+"\n");
			try {
				var tabbrowser=event.target.parentNode.parentNode;
				if(tabbrowser.nodeName=="tabbrowser") {
					var tab=tabbrowser.selectedTab;
					var browser=tabbrowser.getBrowserForTab(tab);
					var document=browser.contentDocument;
					this.updateMenus(document,event.target.ownerDocument.defaultView);
				}
			} catch(e) {
				dump("!!! [Core] handleEvent(select): "+e+"\n");
			}
			break;
		default:
			dump("[Core] handleEvent("+event.type+"/"+event.eventPhase+"/"+event.target+")\n");
	}
}

Core.prototype.cleanupEntriesForDocument=function(document,window) {
	//dump("[Core] cleanupEntriesForDocument("+document.URL+",window)\n");
	try {
		var tbd=[];
		for(var i in this.entries) {
			var entry=this.entries[i];
			var entryType=Util.getPropsString(entry,"entry-type");
			if(entryType=="document") {
				if(window) {
					if(entry.has("window") && entry.has("document")) {
						var entryWindow=entry.get("window",Components.interfaces.nsIDOMWindow);
						var entryDocument=entry.get("document",Components.interfaces.nsIDOMDocument);
						if(entryWindow==window && entryDocument==document) {
							tbd.push(entry);
						}
					}
				} else {
					tbd.push(i);
				}
			}
		}
		for(var i in tbd) {
			this.entries.splice(this.entries.indexOf(tbd[i]),1);
		}
	} catch(e) {
		dump("!!! [Core] cleanupEntriesForDocument: "+e+"\n");
	}
}

Core.prototype.updateEntriesForDocument=function(document,window) {
	//dump("[Core] updateEntriesForDocument("+document.URL+")\n");
	try {
		for(var i in this.probes) {
			var entry=null;
			try {
				entry=this.probes[i].handleDocument(document,window);
			} catch(e) {}
			if(entry) {
				Util.setPropsString(entry,"entry-type","document");
				Util.setPropsString(entry,"document-url",document.URL);
				entry.set("document",document);
				entry.set("window",window);
				if(this.filterBlackList(entry)) {
					this.entries.push(entry);
				}
			}
		}
	} catch(e) {
		dump("!!! [Core] updateEntriesForDocument("+document.URL+"): "+e+"\n");
	}
}

Core.prototype.addEntryForDocument=function(entry,document,window) {
	try {
		//dump("[Core] addEntryForDocument(entry,"+document.URL+",window)\n");
		Util.setPropsString(entry,"entry-type","document");
		Util.setPropsString(entry,"document-url",document.URL);
		entry.set("document",document);
		entry.set("window",window);
		if(this.filterBlackList(entry)) {
			this.smartNamer.updateEntry(entry);
			this.entries.push(entry);
			this.updateMenus(document,window);
		}
	} catch(e) {
		dump("!!! [Core] addEntryForDocument("+document.URL+"): "+e+"\n");
	}
}

Core.prototype.addEntry=function(entry) {
	try {
		//dump("[Core] addEntry(entry)\n");
		var mediaUrl=Util.getPropsString(entry,"media-url");
		if(mediaUrl)
			this.cleanupExpirableEntriesForMediaUrl(mediaUrl);
		Util.setPropsString(entry,"entry-type","expirable");
		Util.setPropsString(entry,"creation-date",""+new Date().getTime());
		if(this.filterBlackList(entry)) {
			this.smartNamer.updateEntry(entry);
			this.entries.push(entry);
			this.updateMenus(null,null);
		}
	} catch(e) {
		dump("!!! [Core] addEntry(): "+e+"\n");
	}
}

Core.prototype.filterBlackList=function(entry) {
	var url=Util.getPropsString(entry,"media-url");
	if(url==null)
		return true;
	for(var i in this.blacklist) {
		if(new RegExp("//[^/]*"+this.blacklist[i]+"/").test(url)) {
			//dump("[Core] filterBlackList(): filtered out "+url+"\n");
			return false;
		}
	}
	return true;
}

Core.prototype.cleanupExpirableEntriesForMediaUrl=function(url) {
	//dump("[Core] cleanupExpirableEntriesForMediaUrl("+url+")\n");
	var tbd=[];
	for(var i=0;i<this.entries.length;i++) {
		var entry=this.entries[i];
		if(Util.getPropsString(entry,"entry-type")=="expirable" &&
				Util.getPropsString(entry,"media-url")==url) {
			//dump("[Core] cleanupExpirableEntriesForMediaUrl("+url+") found\n");
			tbd.push(entry);
		}
	}
	for(var i in tbd)
		this.entries.splice(this.entries.indexOf(tbd[i]),1);	
}

Core.prototype.getTopDocument=function(document) {
	var topDocument=null;
	if(document && document.defaultView) {
		var obj=document.defaultView;
		while(obj) {
			topDocument=obj.document;
			if(obj==obj.parent)
				obj=null;
			else
				obj=obj.parent;
		}
	}
	return topDocument;
}

Core.prototype.updateMenus=function(document,window) {
	//dump("[Core] updateMenus("+(document?document.URL:null)+","+(window?window.content.document.URL:"window")+")\n");
	var topDocument=this.getTopDocument(document);
	try {
		this.cleanupExpiredEntries();
		for(var i in this.regMenus) {
			var menu=this.regMenus[i];
			if(window==null || window==menu.window) {
				if(menu.menupopup) {
					//var menupopup=menu.menupopup.QueryReferent(Components.interfaces.nsIDOMElement);
					var menupopup=menu.menupopup;
					if(menupopup.ownerDocument.defaultView && menupopup.ownerDocument.defaultView.content && 
							menupopup.ownerDocument.defaultView.content.document && 
							menupopup.ownerDocument.defaultView.content.document.URL) {
						pageUrl=menupopup.ownerDocument.defaultView.content.document.URL;
					}
					this.clearMenu(menupopup);
					var gotEntries=false;
					for(var j in this.entries) {
						var entry=this.entries[j];
						if(entry.has("document")) {
							var entryDocument=entry.get("document",Components.interfaces.nsIDOMDocument);
							if(document!=null && topDocument!=null && topDocument!=this.getTopDocument(entryDocument) && entryDocument!=document) {
								continue;
							}
							if(document==null && menu.window.content && menu.window.content.document!=entryDocument) {
								continue;
							}
						}
						var entryWindow=null;
						if(entry.has("window"))
							entryWindow=entry.get("window",Components.interfaces.nsIDOMWindow);
						if(window==null || entryWindow==null || window==entryWindow) {
							var classes=[];
							var menuitem;
							if(this.pref.getBoolPref("extended-download-menu")) {
								menuitem=this.makeDownloadMenu(menupopup,entry,classes);
							} else {
								menuitem=this.makeDownloadMenuitem(menupopup,entry,classes);
							}

							if(entry.has("mouse-listener")) {
								function MouseListener(entry,probeListener) {
									this.entry=entry;
									this.probeListener=probeListener;
								}
								MouseListener.prototype={
									handleEvent: function(event) {
										switch(event.type) {
											case "mouseover":
												this.probeListener.mouseOver(this.entry);
												break;
											case "mouseout":
												this.probeListener.mouseOut(this.entry);
												break;
										}
									}
								}
								var listener=new MouseListener(entry,entry.get("mouse-listener",Components.interfaces.dhIProbeMouseListener));
								var eventTarget=menuitem.QueryInterface(Components.interfaces.nsIDOMNSEventTarget);
								eventTarget.addEventListener("mouseover",listener,false,false);
								eventTarget.addEventListener("mouseout",listener,false,false);
							}
							var debugTooltip=false;
							try {
								debugTooltip=this.pref.getBoolPref("menu-tooltip-debug");
							} catch(e) {}
							if(debugTooltip) {
								var tooltipText="";
								var keys=entry.getKeys({});
								for(var key in keys) {
									tooltipText+="["+keys[key]+"] ";
									var value=Util.getPropsString(entry,keys[key]);
									if(value)
										tooltipText+=value;
									else
										tooltipText+="...";
									tooltipText+="\n";
								}
								menuitem.setAttribute("tooltiptext",tooltipText);								
							} else if(entry.has("media-url")) {
								var mediaUrl=Util.getPropsString(entry,"media-url");
								menuitem.setAttribute("tooltiptext",mediaUrl);
							}
							var highlightMFCP=true;
							try {
								highlightMFCP=this.pref.getBoolPref("highlight-media-from-current-page");
							} catch(e) {}
							if(highlightMFCP && entry.has("page-url")) {
								var pageUrl=Util.getPropsString(entry,"page-url");
								var inCurrentWindow=false;
								if(document) {
									if(document.URL==pageUrl)
										inCurrentWindow=true;
								} else {
									if(menu.window.content && menu.window.content.document.URL==pageUrl)
										inCurrentWindow=true;
								}
								if(inCurrentWindow)
									classes.push("dwhelper-mediainpage");
							}
							menuitem.setAttribute("class",classes.join(" "));
							menupopup.appendChild(menuitem);
							gotEntries=true;
						}
					}
					if(menu.button) {
						menu.button.removeAttribute("image");
						var butClass=menu.button.getAttribute("class");
						butClass=butClass.replace(/ on-noanim| on| off/,"");
						if(gotEntries) {
							var ia=true;
							try {
								ia=this.pref.getBoolPref("icon-animation");
							} catch(e) {}
							if(ia)
								butClass+=" on";
							else
								butClass+=" on-noanim";
							menu.button.setAttribute("type","menu-button");
						} else {
							butClass+=" off";
							menu.button.removeAttribute("type");
						}
						menu.button.setAttribute("class",butClass);
					}
					if(menu.hideParentIfEmpty && menupopup.parentNode) {
						if(gotEntries)
							menupopup.parentNode.setAttribute("hidden","false");
						else
							menupopup.parentNode.setAttribute("hidden","true");
					}
				}
			} 
		}
	} catch(e) {
		dump("!!! [Core] updateMenus(): "+e+"\n");
	}
}

Core.prototype.makeDownloadMenuitem=function(menupopup,entry,classes) {
	var menuitem=menupopup.ownerDocument.createElement("menuitem");
	var eventTarget=menuitem.QueryInterface(Components.interfaces.nsIDOMNSEventTarget);
	var label=Util.getPropsString(entry,"label");
	if(label)
		menuitem.setAttribute("label",label);
	var icon=Util.getPropsString(entry,"icon-url");
	if(icon) {
		menuitem.setAttribute("image",icon);
		classes.push("menuitem-iconic");
		classes.push("controler-entry");
	}
	
	function CommandListener(entry,commandListener) {
		this.entry=entry;
		this.commandListener=commandListener;
	}
	CommandListener.prototype={
		handleEvent: function(event) {
			if(event.type=="command") {
				var key=(event.ctrlKey?CTRL_KEY:0) |
					(event.shiftKey?SHIFT_KEY:0) |
					(event.altKey?ALT_KEY:0) |
					(event.metaKey?META_KEY:0);
				this.commandListener.handleCommand(this.entry,key);
			}
		}
	}
	var commandListener=new CommandListener(entry,this);
	eventTarget.addEventListener("command",commandListener,false,false);
	return menuitem;
}

Core.prototype.makeDownloadMenu=function(menupopup,entry,classes) {
	var uiDocument=menupopup.ownerDocument;
	var menuitem=uiDocument.createElement("menu");
	var menupopup1=uiDocument.createElement("menupopup");
	menuitem.appendChild(menupopup1);

	function CommandListener(core,entry,processor) {
		this.core=core;
		this.entry=entry;
		this.processor=processor;
	}
	CommandListener.prototype={
		handleEvent: function(event) {
			try {
				this.core.processEntry(this.processor,this.entry);
			} catch(e) {
				dump("!!! [Core/DownloadMenu] CommandListener.handleEvent(): "+e+"\n");
			}
			event.stopPropagation(); 
		}
	}
	
	var i=this.getProcessors().enumerate();
	while(i.hasMoreElements()) {
		var processor=i.getNext().QueryInterface(Components.interfaces.dhIProcessor);
		if(processor.canHandle(entry)) {
			var menuitem1=uiDocument.createElement("menuitem");
			menuitem1.setAttribute("label",processor.title);
			menuitem1.setAttribute("tooltiptext",processor.description);
			menuitem1.setAttribute("class","download-processor-entry");
			menuitem1.QueryInterface(Components.interfaces.nsIDOMNSEventTarget).
				addEventListener("command",new CommandListener(this,entry,processor),false,false);
			menupopup1.appendChild(menuitem1);
		}
	}

	var eventTarget=menuitem.QueryInterface(Components.interfaces.nsIDOMNSEventTarget);
	var label=Util.getPropsString(entry,"label");
	if(label)
		menuitem.setAttribute("label",label);
	var icon=Util.getPropsString(entry,"icon-url");
	if(icon) {
		menuitem.setAttribute("image",icon);
		classes.push("menu-iconic");
		classes.push("controler-entry");
	}
	
	function ClickListener(entry,commandListener) {
		this.entry=entry;
		this.commandListener=commandListener;
	}
	ClickListener.prototype={
		handleEvent: function(event) {
			if(event.target.localName=="menu" && event.type=="click") {
				var key=(event.ctrlKey?CTRL_KEY:0) |
					(event.shiftKey?SHIFT_KEY:0) |
					(event.altKey?ALT_KEY:0) |
					(event.metaKey?META_KEY:0);
				event.target.parentNode.hidePopup();
				this.commandListener.handleCommand(this.entry,key);
			}
		}
	}
	var commandListener=new ClickListener(entry,this);
	eventTarget.addEventListener("click",commandListener,false,false);	
	return menuitem;
}

Core.prototype.clearMenu=function(menupopup) {
	this.deepRemoveChildren(menupopup);
}

Core.prototype.deepRemoveChildren=function(element) {
	if(element==null)
		return;
	while(element.firstChild) {
		var child=element.firstChild;
		this.deepRemoveChildren(child);
		element.removeChild(child);
	}
}

Core.prototype.cleanupExpiredEntries=function() {
	//dump("[Core] cleanupExpiredEntries()\n");
	var expireTimeout=60;
	try {
		expireTimeout=this.pref.getIntPref("menu-expiration");
	} catch(e) {}
	var timeNow=new Date().getTime();
	var tbd=[];
	for(var i=0;i<this.entries.length;i++) {
		var entry=this.entries[i];
		if(Util.getPropsString(entry,"entry-type")=="expirable") {
			var created=parseInt(Util.getPropsString(entry,"creation-date"));
			if(timeNow>created+expireTimeout*1000) {
				//dump("[Core] cleanupExpiredEntry(): removed "+Util.getPropsString(entry,"media-url")+"\n");
				tbd.push(entry);
			}
		}
	}
	for(var i in tbd)
		this.entries.splice(this.entries.indexOf(tbd[i]),1);	
}

Core.prototype.registerWindow=function(window) {
	//dump("[Core] registerWindow("+window+")\n");
	this.monitorWindow(window);
}
	
Core.prototype.unregisterWindow=function(window) {
	//dump("[Core] unregisterWindow("+window+")\n");
	try {
		var tbd=[];
		for(var i in this.entries) {
			var entry=this.entries[i];
			if(entry.has("window")) {
				var entryWindow=entry.get("window",Components.interfaces.nsIDOMWindow);
				if(entryWindow==window) {
					tbd.push(entry);
				}
			}
		}
		for(var i in tbd) {
			this.entries.splice(this.entries.indexOf(tbd[i]),1);
		}
	} catch(e) {
		dump("!!! [Core] unregisterWindow: "+e+"\n");
	}
}
	
Core.prototype.monitorWindow=function(win) {
	try {

		//dump("[Core] monitorWindow("+win+")\n");

		if(win.gBrowser) {
			var mPanCont=win.gBrowser.mPanelContainer;
			if(mPanCont) {
				mPanCont=mPanCont.QueryInterface(Components.interfaces.nsIDOMNSEventTarget);
				mPanCont.addEventListener("select",this,false,false);
			}
		}
		
		function Listener(core) {
			this.core=core;
		}
		Listener.prototype={
			handleEvent: function(event) {
				if(event.target.id=="contentAreaContextMenu" || event.target.getAttribute("dwhelper-monitored-context-submenu")=="true")
					this.core.contextMenuOpened(event);
			}
		}
		win.document.getElementById("contentAreaContextMenu").
			QueryInterface(Components.interfaces.nsIDOMNSEventTarget).
			addEventListener("popupshowing",new Listener(this),false,false);

		win=win.QueryInterface(Components.interfaces.nsIDOMNSEventTarget);
		//win.addEventListener("load",this,false,false);
		
		function WindowMonitor(window,core) {
			this.window=window;
			this.core=core;
		}
		WindowMonitor.prototype={
			handleEvent: function(event) {
				switch(event.type) {
					case "pageshow":
						try {
							//dump("pageshow - "+event.target.URL+" "+event.target+"\n");
							var document=event.target.QueryInterface(Components.interfaces.nsIDOMHTMLDocument);
							this.core.updateEntriesForDocument(document,this.window);
							try {
								var hook=Components.classes["@downloadhelper.net/dom-hook;1"]
								                            .getService(Components.interfaces.dhIDOMHook);
								hook.hook(document);
							} catch(e) {
								dump("!!! [Core] monitorWindow/hook: "+e+"\n");
							}
							this.core.updateMenus(document,this.window);
						} catch(e) {}
						break;
					case "pagehide":
						try {
							//dump("pagehide - "+event.target.URL+"\n");
							var document=event.target;
							this.core.cleanupEntriesForDocument(document,this.window);
							this.core.updateMenus(document,this.window);
						} catch(e) {}
						break;
				}
			}
		}
		var windowMonitor=new WindowMonitor(win,this)
		win.addEventListener("pageshow",windowMonitor,false,false);
		win.addEventListener("pagehide",windowMonitor,false,false);
		
	} catch(e) {
		dump("!!! [Core] monitorWindow: "+e+"\n");
	}
}

Core.prototype.registerProcessor=function(processor) {
	for(var i in this.processors) {
		if(this.processors[i].name==processor.name) {
			this.processors[i]=processor;
			return;
		}
	}
	this.processors.push(processor);
}

Core.prototype.unregisterProcessor=function(processor) {
	this.processors.splice(this.processors.indexOf(processor),1);
}

Core.prototype.handleCommand=function(entry,key) {
	try {
		//dump("[Core] handleCommand(entry,"+key+")\n");
		var processor=null;
		var procName=this.processorKeyMap[key];
		if(procName) {
			for(var i in this.processors) {
				if(this.processors[i].enabled && this.processors[i].name==procName) {
					processor=this.processors[i];
					break;
				}
			}
		}
		if(processor==null) {
	        var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
	                                    .getService(Components.interfaces.nsIWindowMediator);
			var w = wm.getMostRecentWindow("navigator:browser");
			var processors=[];
			for(var i in this.processors) {
				if(this.processors[i].enabled) {
					processors.push({
						processor: this.processors[i],
						canHandle: this.processors[i].canHandle(entry)
					});
				}
			}
			var data={ 
				processors: processors,
			};
			w.openDialog('chrome://dwhelper/content/pick-processor.xul','dwhelper-dialog',"chrome,centerscreen,modal",data);
			//dump("[Core] handleCommand(entry,"+key+") processor: "+data.processor+"\n");
			var procName=data.processor;
			for(var i in this.processors) {
				if(this.processors[i].enabled && this.processors[i].name==procName) {
					processor=this.processors[i];
					break;
				}
			}
		}
		if(processor==null)
			return;
		this.processEntry(processor,entry);
	} catch(e) {
		dump("!!! [Core] handleCommand(entry,"+key+"): "+e+"\n");
	}
}

Core.prototype.updateBlackList=function() {
	var blacklistPref=this.pref.getCharPref("media-host-blacklist");
	this.blacklist=blacklistPref.split("|");
	var entries=[];
	var needUpdate=false;
	for(var i in this.entries) {
		if(this.filterBlackList(this.entries[i]))
			entries.push(this.entries[i]);
		else
			needUpdate=true;
	}
	this.entries=entries;
	if(needUpdate)
		this.updateMenus(null,null);
}

Core.prototype.shareBlackList=function() {
	if(this.pref.getBoolPref("share-blacklist")) {
		var now=new Date().getTime();
		var lastShared=parseInt(this.pref.getCharPref("last-shared-blacklist"));
		if(now-lastShared<1000*60*60*24*7)
			return;
		this.pref.setCharPref("last-shared-blacklist",""+now);
		var newDomains=[];
		function map(arr) {
			var obj={};
			for(var i in arr) obj[arr[i]]=1;
			return obj;
		}
		var currentDomains=map(this.pref.getCharPref("media-host-blacklist").split("|"));
		var lastDomains=map(this.pref.getCharPref("last-media-host-blacklist").split("|"));
		for(var domain in currentDomains) {
			if(!(domain in lastDomains)) {
				newDomains.push(domain);
			}
		}
		this.pref.setCharPref("last-media-host-blacklist",this.pref.getCharPref("media-host-blacklist"));
		if(newDomains.length>0) {
			var xml="<?xml version='1.0'?>\n<blacklist-domains>\n";
			for(var i in newDomains) {
				xml+="  <domain>"+newDomains[i]+"</domain>\n";
			}
			xml+="</blacklist-domains>";
	        var xmlhttp = Components.classes["@mozilla.org/xmlextras/xmlhttprequest;1"].
	        	createInstance(Components.interfaces.nsIXMLHttpRequest);
	        xmlhttp.open("POST", "http://www.downloadhelper.net/share-blacklist.php")
	        xmlhttp.send(xml);
		}
	}
}

Core.prototype.updateProcessorKeyMap=function() {
	//dump("[Core] updateProcessorKeyMap()\n");
	this.processorKeyMap={}
	var keymap="0:download,2:convert-choice,3:quick-download";
	try {
		keymap=this.pref.getCharPref("processor-keymap");
	} catch(e) {}
	var parts=keymap.split(",");
	for(var i in parts) {
		var parts2=parts[i].split(":");
		if(parts2.length==2 && /^[0-9]+$/.test(parts2[0]) && parts2[1].length>0) {
			this.processorKeyMap[parseInt(parts2[0])]=parts2[1];
		}
	}
	//dump("[Core] updateProcessorKeyMap():\n");
	//this.dumpObject(this.processorKeyMap);
}

Core.prototype.downloadFinished=function(status, request, entry, ctx) {
	//dump("[Core] downloadFinished("+status+",...)\n");
	if(status==0) {
		var format=Util.getPropsString(entry,"format");
		if(format) {
			var file;
			if(entry.has("cv-file")) {
				file=entry.get("cv-file",Components.interfaces.nsILocalFile);
			} else {
			 	file=Components.classes["@mozilla.org/file/directory_service;1"]
			 	                        .getService(Components.interfaces.nsIProperties)
			 	                        .get("TmpD", Components.interfaces.nsILocalFile);
			 	file.append("dwhelper-cv");
			 	file.createUnique(Components.interfaces.nsIFile.NORMAL_FILE_TYPE, 0644);
			 	entry.set("cv-file",file);				
			}
			this.cvMgr.addConvert(entry.get("dl-file",Components.interfaces.nsILocalFile),file,format,true,this,entry,ctx);
		} else {
			var processor=ctx.QueryInterface(Components.interfaces.dhIProcessor);
			processor.handle(entry);
		}
	}
}

Core.prototype.conversionFinished=function(status, entry, ctx) {
	//dump("[Core] conversionFinished("+status+",...)\n");
	if(status) {
		var processor=ctx.QueryInterface(Components.interfaces.dhIProcessor);
		processor.handle(entry);
	}
}

Core.prototype.getProcessors=function() {
	var processors=Components.classes["@mozilla.org/array;1"].
		createInstance(Components.interfaces.nsIMutableArray);
	for(var i in this.processors) {
		if(this.processors[i].enabled)
			processors.appendElement(this.processors[i],false);
	}
	return processors.QueryInterface(Components.interfaces.nsIArray)
}

Core.prototype.updateSystemMenu=function(menupopup) {
	
	try {
	
	this.clearMenu(menupopup);
		
	var systemMenuData=[
		{ 
			label: Util.getText("menu.preferences"),
			image: "chrome://dwhelper/skin/icon-pref.png",
			type: "dialog",
			modal: false,
			toolbar: true,
			url: "chrome://dwhelper/content/preferences-new.xul"
		},
		{ 
			label: Util.getText("menu.sites"),
			type: "dialog",
			url: "chrome://dwhelper/content/sites.xul"
		},
		{ 
			label: Util.getText("menu.history"),
			type: "dialog",
			modal: false,
			url: "chrome://dwhelper/content/history.xul",
			cond: "isHistoryEnabled"
		},
		{ 
			label: Util.getText("menu.convert-register"),
			image: "chrome://dwhelper/skin/converter-12x12.png",
			type: "dialog",
			modal: false,
			url: "chrome://dwhelper/content/convert-register.xul",
			cond: "conversionNeedsRegistration"
		},
		{ 
			label: Util.getText("menu.manual-convert"),
			image: "chrome://dwhelper/skin/converter-12x12.png",
			type: "method",
			method: "manualConvert",
			cond: "isConversionEnabled"
		},
		{ 
			label: Util.getText("menu.converter-queue"),
			image: "chrome://dwhelper/skin/converter-12x12.png",
			type: "dialog",
			modal: false,
			url: "chrome://dwhelper/content/converter-queue.xul",
			cond: "isConversionEnabled"
		},
		{ 
			label: Util.getText("menu.download-queue"),
			type: "dialog",
			modal: false,
			url: "chrome://dwhelper/content/download-queue.xul",
			cond: "useDownloadQueue"
		},
		{ 
			label: Util.getText("menu.open-download-dir"),
			type: "method",
			method: "openDownloadDirectory",
		},
		{ 
			label: Util.getText("menu.search-videos"),
			type: "method",
			method: "searchVideos",
		},
		{ 
			label: Util.getText("menu.search-adult-videos"),
			type: "method",
			method: "searchAdultVideos",
			cond: "isAdultEnabled"
		},
		{ 
			label: Util.getText("mp3tunes.label.open-mp3tunes-locker"),
			image: "chrome://dwhelper/skin/mp3tunes-16x16.png",
			type: "method",
			method: "openMP3TunesLocker",
			cond: "isMP3TunesEnabled"
		},
		{ 
			label: Util.getText("menu.help"),
			image: "chrome://dwhelper/skin/icon-help.png",
			type: "opentab",
			url: "http://www.downloadhelper.net/manual.php",
		},
		{ 
			label: Util.getText("menu.knowledge-base"),
			type: "opentab",
			url: "http://www.downloadhelper.net/support-kb.php",
		},
		{ 
			label: Util.getText("menu.tutorial-videos"),
			type: "opentab",
			url: "http://www.downloadhelper.net/tutorials.php",
		},
		{ 
			type: "separator"
		},
		{ 
			label: Util.getText("menu.subtile.extension"),
			image: "chrome://dwhelper/skin/icon-subtile.png",
			type: "menu",
			menu: [
				{
					label: Util.getText("menu.subtile.install"),
					type: "opentab",
					url: "http://www.downloadhelper.net/dh-st-install.php",
				},
				{
					label: Util.getText("menu.subtile.monitor-video-sites"),
					type: "opentab",
					url: "http://www.downloadhelper.net/dh-monitor-vsites.php",
				},
				{
					label: Util.getText("menu.subtile.cust-monitor-video-sites"),
					type: "opentab",
					url: "http://www.downloadhelper.net/dh-st-build.php",
				},
				{
					label: Util.getText("menu.subtile.create-menu"),
					type: "opentab",
					url: "http://www.downloadhelper.net/dh-create.php",
	   			},
	   		]
	   	},
		{ 
			label: Util.getText("menu.media"),
			type: "downloadmenu",
		},
		{ 
			type: "separator"
		},
		{ 
			label: Util.getText("menu.about"),
			image: "chrome://dwhelper/skin/icon-about.png",
			type: "dialog",
			modal: false,
			url: "chrome://dwhelper/content/about.xul"
	   	}
	];
	this.updateSystemMenuLevel(menupopup,systemMenuData);
	} catch(e) {
		dump("!!! [Core] updateSystemMenu(): "+e+"\n");
	}
}

Core.prototype.updateSystemMenuLevel=function(menupopup,menuData) {	
	var document=menupopup.ownerDocument;
	var useIcons=this.pref.getBoolPref("system-menu-icons");
	for(var i in menuData) {
		var mData=menuData[i];
		switch(mData.type) {
			case "dialog":
			case "method":
			case "opentab":
				if(mData.cond) {
					if(!this[mData.cond](mData))
						continue;
				}
				function Listener(core,data) {
					this.core=core;
					this.data=data;
				}
				Listener.prototype={
					handleEvent: function(event) {
						if(event.type=="command") {
					        var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
					                                    .getService(Components.interfaces.nsIWindowMediator);
							var window = wm.getMostRecentWindow("navigator:browser");
							switch(this.data.type) {
								case "dialog":
								    var options="chrome,centerscreen,titlebar,resizable=yes";
								    if(this.data.toolbar==null || this.data.toolbar!=false)
								    	options+=",toolbar";
								    if(this.data.modal==null || this.data.modal!=false)
								    	options+=",modal";
								    window.openDialog(this.data.url,'_blank',options, {} );
								    break;
								case "method":
									this.core[this.data.method](this.data);
									break;
								case "opentab":
									var browser=window.getBrowser();
									browser.selectedTab=browser.addTab(this.data.url);
									break;
							}
						}
					}
				}
				var classes=[];
				var menuitem=document.createElement("menuitem");
				menuitem.setAttribute("label",mData.label);
				if(mData.image) {
					if(useIcons) {
						classes.push("menuitem-iconic");
						menuitem.setAttribute("image",mData.image);
					}
				}
				menuitem.setAttribute("class",classes.join(" "));
				menupopup.appendChild(menuitem);
				menuitem=menuitem.QueryInterface(Components.interfaces.nsIDOMNSEventTarget);
				menuitem.addEventListener("command",new Listener(this,mData),false,false);
				break;
				
			case "separator":
				var menuseparator=document.createElement("menuseparator");
				menupopup.appendChild(menuseparator);				
				break;
				
			case "menu":
				var classes=[];
				var menu=document.createElement("menu");
				menu.setAttribute("label",mData.label);
				if(mData.image) {
					if(useIcons) {
						classes.push("menu-iconic");
						menu.setAttribute("image",mData.image);
					}
				}
				menu.setAttribute("class",classes.join(" "));
				menupopup.appendChild(menu);
				var menupopup0=document.createElement("menupopup");
				menu.appendChild(menupopup0);
				this.updateSystemMenuLevel(menupopup0,mData.menu);
				break;

			case "downloadmenu":
				var menu=document.createElement("menu");
				menu.setAttribute("label",mData.label);
				menupopup.appendChild(menu);
				var menupopup0=document.createElement("menupopup");
				menupopup0.setAttribute("hide-parent-if-empty","true");
				menu.appendChild(menupopup0);
				function Listener(core,menupopup,target) {
					this.core=core;
					this.menupopup=menupopup;
					this.target=target;
				}
				Listener.prototype={
					handleEvent: function(event) {
						if(event.type=="popuphiding" && event.target==this.target) {
							this.core.unregisterMenu(this.menupopup);
							this.menupopup.parentNode.parentNode.removeChild(this.menupopup.parentNode);
						}
					}
				}
				menupopup.addEventListener("popuphiding",new Listener(this,menupopup0,menupopup),false,false);
				this.registerMenu(menupopup0,Components.interfaces.dhICore.MENU_TYPE_DOWNLOAD);
				break;
		}
	}
}

Core.prototype.isHistoryEnabled=function(data) {
	var he=false;
	try {
		he=this.pref.getBoolPref("history-enabled");
	} catch(e) {}
	return he;
}

Core.prototype.conversionNeedsRegistration=function(data) {
	var cvInfo=this.cvMgr.getInfo();
	if(!cvInfo.get("windows",Components.interfaces.nsISupportsPRBool).data)
		return false;
	if(!cvInfo.get("enabled",Components.interfaces.nsISupportsPRBool).data)
		return false;
	return cvInfo.get("unregistered",Components.interfaces.nsISupportsPRBool).data;
}

Core.prototype.isConversionEnabled=function(data) {
	var cvInfo=this.cvMgr.getInfo();
	return cvInfo.get("enabled",Components.interfaces.nsISupportsPRBool).data;
}

Core.prototype.isMP3TunesEnabled=function(data) {
	return this.pref.getBoolPref("mp3tunes.enabled");
}

Core.prototype.isOneByOne=function(data) {
	var dm="onebyone";
	try {
		dm=this.pref.getCharPref("download-mode");
	} catch(e) {}
	return (dm=="onebyone");
}

Core.prototype.useDownloadQueue=function(data) {
	var dm="onebyone";
	try {
		dm=this.pref.getCharPref("download-mode");
	} catch(e) {}
	return (dm=="onebyone" || dm=="controlled");
}

Core.prototype.openDownloadDirectory=function(data) {
	//dump("openDownloadDirectory()\n");
	try {
		var dir=this.dlMgr.getDownloadDirectory().QueryInterface(Components.interfaces.nsILocalFile);
		dir.reveal();
	} catch(e) {}
}

Core.prototype.openMP3TunesLocker=function(data) {
	function AuthObserver() {
	}
	AuthObserver.prototype={
		observe: function(subject,topic,data) {
	        var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
	                                    .getService(Components.interfaces.nsIWindowMediator);
			var window = wm.getMostRecentWindow("navigator:browser");
			if(topic=="mp3tunes-auth-succeeded") {
				var browser=window.getBrowser();
				browser.selectedTab=browser.addTab("http://www.mp3tunes.com/player/");
			} else {
				window.alert(Util.getText("mp3tunes.error.no-login-check-credentials"));
			}
		}
	}
	var mtMgr=Components.classes["@downloadhelper.net/mp3tunes-manager;1"]
	                          	.getService(Components.interfaces.dhIMP3Tunes);
	var username=this.pref.getCharPref("mp3tunes.username");
	var password=Util.getPassword("mp3tunes");
	if(password==null) password="";
	mtMgr.authenticate(username,password,new AuthObserver());
}

Core.prototype.openDownloadDirectoryCommand=function() {
	//dump("[Core] openDownloadDirectoryCommand()\n");
	this.openDownloadDirectory(null);
}

Core.prototype.quickDownloadCommand=function() {
	//dump("[Core] quickDownloadCommand()\n");
	try {
		var wwatch = Components.classes["@mozilla.org/embedcomp/window-watcher;1"].getService().
			QueryInterface(Components.interfaces.nsIWindowWatcher);
		var aWindow=wwatch.activeWindow;
		if(aWindow==null || aWindow.content==null || aWindow.content.document==null || aWindow.content.document.URL==null)
			return;
		var url=aWindow.content.document.URL;
		var foundEntry=null;
		var mostRecent=0;
		for(var i in this.entries) {
			var entry=this.entries[i];
			if(Util.getPropsString(entry,"entry-type")=="expirable" &&
				Util.getPropsString(entry,"page-url")==url) {
				var creationDate=parseInt(Util.getPropsString(entry,"creation-date"));
				if(creationDate>mostRecent) {
					creationDate=mostRecent;
					foundEntry=entry;
				}
			}
		}
		if(foundEntry) {
			this.quickProcess(foundEntry);
		} else {
			this.promptService.alert(null,"DownloadHelper",Util.getText("error.quickkey.nohit"));
		}
	} catch(e) {
		dump("!!! [Core] quickDownloadCommand(): "+e+"\n");
	}
}

Core.prototype.manualConvert=function(data) {
	try {
        var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
                                    .getService(Components.interfaces.nsIWindowMediator);
		var window = wm.getMostRecentWindow("navigator:browser");
		var filePicker=Components.classes["@mozilla.org/filepicker;1"]
		                                  .createInstance(Components.interfaces.nsIFilePicker);
       	filePicker.init(window,Util.getText("title.files-to-convert"), 
       		Components.interfaces.nsIFilePicker.modeOpenMultiple);
       	var dir=this.dlMgr.getDownloadDirectory();
       	filePicker.displayDirectory=dir;
       	filePicker.appendFilter("FLV","*.flv; *.FLV");
       	filePicker.appendFilter("Video","*.flv; *.FLV; *.avi; *.AVI; *.mpeg; *.MPEG; *.mpg; *.MPG; *.wmv; *.WMV; *.rm; *.RM; *.mov; *.MOV; *.mp4; *.MP4");
       	filePicker.appendFilters(Components.interfaces.nsIFilePicker.filterAll);
       	var r=filePicker.show();
       	if(r==Components.interfaces.nsIFilePicker.returnCancel)
       		return;
       		
    	var data={};
		window.openDialog("chrome://dwhelper/content/convert-manual.xul",
	                 "dwhelper-convert-manual", "chrome,centerscreen,modal",data);
	    if(data.format==null) {
	    	return;
	    }

       	var i=filePicker.files;
       	while(i.hasMoreElements()) {
       		var file=i.getNext().QueryInterface(Components.interfaces.nsIFile);
       		var targetFile=file.parent;
       		targetFile.append(this.cvMgr.getConvertedFileName(file.leafName,data.format));
       		this.cvMgr.addConvert(file,targetFile,data.format,false,null,null,null);
       	}
	} catch(e) {
		dump("!!! [Core] manualConvert():"+e+"\n");
	}
}

Core.prototype.buttonClicked=function(window) {
	var action=this.pref.getCharPref("icon-click");
	if(action=="sites") {
	    var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
	                                .getService(Components.interfaces.nsIWindowMediator);
		var window = wm.getMostRecentWindow("navigator:browser");
		window.open("chrome://dwhelper/content/sites.xul",
	            "dwhelper-sites", "chrome,centerscreen,resizable=yes").focus();
	} else if(action=="open-popup" && !Util.priorTo19()) {
		var document=window.document;
		var button=document.getElementById("dwhelper-button");
		var menupopup=document.getElementById("dwhelper-button-menupopup");
		menupopup.openPopup(button,"after_start",0,0,false,false);
	} else if(action=="quick-download") {
		this.quickDownloadCommand();
	}
}

Core.prototype.registerContextItem=function(item) {
	//dump("[Core] registerContextItem("+item.tagName+")\n");
	try {
		var service=Components.classes[item.getAttribute("context-item-handler")].
			getService(Components.interfaces.dhIContextItem);
		var itemData={
				item: item,
				window: item.ownerDocument.defaultView,
				service: service 
		};
		this.ctxItems.push(itemData);
		function Listener(itemData) {
			this.itemData=itemData;
		}
		Listener.prototype={
			handleEvent: function(event) {
				var window=event.target.ownerDocument.defaultView;
				this.itemData.service.handle(window.content.document,window,this.itemData.item);
			}
		}
		item=item.QueryInterface(Components.interfaces.nsIDOMNSEventTarget);
		item.addEventListener("command",new Listener(itemData),false,false);
	} catch(e) {
		dump("!!! [Core] registerContextItem(): "+e+"\n");
	}
}

Core.prototype.unregisterContextItem=function(item) {
	//dump("[Core] unregisterContextItem("+item.tagName+")\n");
	for(var i in this.ctxItems) {
		if(this.ctxItems[i].item==item) {
			this.ctxItem.splice(i,1);
			break;
		}
	}
}

Core.prototype.contextMenuOpened=function(event) {
	//dump("[Core] contextMenuOpened()\n");
	try {
		var window=event.target.ownerDocument.defaultView;
		for(var i in this.ctxItems) {
			var ctxItem=this.ctxItems[i];
			if(window==ctxItem.window) {
				var canHandle=ctxItem.service.canHandle(window.content.document,window,ctxItem.item);
				if(canHandle)
					ctxItem.item.setAttribute("hidden","false");
				else
					ctxItem.item.setAttribute("hidden","true");
			}
		}
	} catch(e) {
		dump("!!! [Core] contextMenuOpened(): "+e+"\n");
	}
}

Core.prototype.quickProcess=function(entry) {
	//dump("[Core] quickProcess()\n");
	var quickProcessor="quick-download";
	try {
	} catch(e) {
		quickProcessor=this.pref.getCharPref("quick-processor");
	}
	var processor=null;
	for(var i in this.processors) {
		if(this.processors[i].enabled && this.processors[i].name==quickProcessor) {
			processor=this.processors[i];
			break;
		}
	}
	if(processor==null) {
		dump("!!! [Core] quickProcess(): no processor for "+quickProcessor+"\n");
		return;
	}
	this.processEntry(processor,entry);
}

Core.prototype.processEntry=function(processor,entry) {
	entry=this.cloneEntry(entry);
	if(processor.canHandle(entry)) {
		if(processor.requireDownload(entry)) {
			if(processor.preDownload(entry)==false)
				return;
			var mediaUrl=Util.getPropsString(entry,"media-url");
			if(mediaUrl)
				this.listMgr.addCurrentURL(mediaUrl);
			this.dlMgr.download(this,entry,processor);
		} else {
			processor.handle(entry);
		}
	}	
}

Core.prototype.cloneEntry=function(entry) {
	var entry0=Components.classes["@mozilla.org/properties;1"].
		createInstance(Components.interfaces.nsIProperties);
	var keys=entry.getKeys({});
	for(var i in keys) {
		var key=keys[i];
		var value;
		try {
			value=entry.get(key,Components.interfaces.nsIArray);
			var array0=Components.classes["@mozilla.org/array;1"].
				createInstance(Components.interfaces.nsIMutableArray);
			var j=value.enumerate();
			while(j.hasMoreElements()) {
				var entry1=j.getNext().QueryInterface(Components.interfaces.nsIProperties);
				array0.appendElement(this.cloneEntry(entry1),false);
			}
			value=array0;
		} catch(e) {
			value=entry.get(key,Components.interfaces.nsISupports);
		}
		entry0.set(key,value);
	}
	return entry0;
}

Core.prototype.removeDownloadCookie=function() {
	try {
		var cMgr = Components.classes["@mozilla.org/cookiemanager;1"].
           getService(Components.interfaces.nsICookieManager);
		try {
			cMgr.remove(".downloadhelper.net","dwcount","/",false);
		} catch(e) {}
		try {
			cMgr.remove(".vidohe.com","dwcount","/",false);
		} catch(e) {}
	} catch(e) {
	}

}

Core.prototype.getEntriesForDocument=function(document) {
	var entries=Components.classes["@mozilla.org/array;1"].
		createInstance(Components.interfaces.nsIMutableArray);
	for(var i in this.entries) {
		if(this.entries[i].has("document")) {
			var document0=this.entries[i].get("document",Components.interfaces.nsIDOMDocument);
			if(document0==document) {
				entries.appendElement(this.entries[i],false);
			}
		}
	}
	return entries.QueryInterface(Components.interfaces.nsIArray)
}

Core.prototype.getEntries=function() {
	var entries=Components.classes["@mozilla.org/array;1"].
		createInstance(Components.interfaces.nsIMutableArray);
	for(var i in this.entries) {
		entries.appendElement(this.entries[i],false);
	}
	return entries.QueryInterface(Components.interfaces.nsIArray)
}

Core.prototype.searchVideos=function() {
	this.doSearchVideos(false);
}

Core.prototype.searchAdultVideos=function() {
	this.doSearchVideos(true);
}

Core.prototype.doSearchVideos=function(adult) {
	var dialogTitle=Util.getText(adult?"title.search-adult-videos":"title.search-videos");
	var query={}
	if(!this.promptService.prompt(null,dialogTitle,Util.getText("message.search-videos"),query,null,{}))
		return;
    var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
                                .getService(Components.interfaces.nsIWindowMediator);
    var window = wm.getMostRecentWindow("navigator:browser");
	var browser=window.getBrowser();
    var url;
    if(adult)
    	url="http://www.downloadhelper.net/videox-search-results.php?cx=005536796155304041479%3Ar9ep8ygv2ca&cof=FORID%3A11&from=dh-adult";
    else
    	url="http://www.downloadhelper.net/video-search-results.php?cx=005536796155304041479%3Ahbixpuuu7l8&cof=FORID%3A11&from=dh-family";
    url+="&q="+window.escape(query.value);
	browser.selectedTab=browser.addTab(url);
}

Core.prototype.updateSmartName=function() {
	for(var i in this.entries)
		this.smartNamer.updateEntry(this.entries[i]);
	this.updateMenus(null,null);	
}

Core.prototype.isAdultEnabled=function() {
	var allowAdult=this.pref.getBoolPref("adult");
	var safeMode=false;
	try {
		safeMode=this.pref.getBoolPref("safe-mode");
	} catch(e) {}
	return safeMode==false && allowAdult;
}


Core.prototype.dumpObject=function(obj) {
	dump(obj+"\n");
	for(var field in obj) {
		dump("  "+field+": ");
		try {
		if(typeof(obj[field])=="function")
			dump("()");
		else
			dump(obj[field]);
		 } catch(e) { dump("!!!"); }
		dump("\n");
	}
}

Core.prototype.QueryInterface = function(iid) {
	//dump("[Core] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhICore)==false &&
    	iid.equals(Components.interfaces.dhIDownloadListener)==false &&
    	iid.equals(Components.interfaces.dhIConversionListener)==false &&
    	iid.equals(Components.interfaces.nsIObserver)==false &&
    	iid.equals(Components.interfaces.nsIDOMEventListener)==false &&
    	iid.equals(Components.interfaces.nsISupports)==false
	) {
            throw Components.results.NS_ERROR_NO_INTERFACE;
        }
    return this;
}

var vCoreModule = {
    firstTime: true,
    
    /*
     * RegisterSelf is called at registration time (component installation
     * or the only-until-release startup autoregistration) and is responsible
     * for notifying the component manager of all components implemented in
     * this module.  The fileSpec, location and type parameters are mostly
     * opaque, and should be passed on to the registerComponent call
     * unmolested.
     */
    registerSelf: function (compMgr, fileSpec, location, type) {

        if (this.firstTime) {
            this.firstTime = false;
            throw Components.results.NS_ERROR_FACTORY_REGISTER_AGAIN;
        }
        compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
        compMgr.registerFactoryLocation(NS_CORE_CID,
                                        "Core",
                                        NS_CORE_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_CORE_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_CORE_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vCoreFactory;
    },

    /* factory object */
    vCoreFactory: {
        /*
         * Construct an instance of the interface specified by iid, possibly
         * aggregating it with the provided outer.  (If you don't know what
         * aggregation is all about, you don't need to.  It reduces even the
         * mightiest of XPCOM warriors to snivelling cowards.)
         */
        createInstance: function (outer, iid) {
            if (outer != null) {
				throw Components.results.NS_ERROR_NO_AGGREGATION;
	    	}
	
	    	if(Util==null) 
	    		Util=Components.classes["@downloadhelper.net/util-service;1"]
					.getService(Components.interfaces.dhIUtilService);

			return new Core().QueryInterface(iid);
        }
    },

    /*
     * The canUnload method signals that the component is about to be unloaded.
     * C++ components can return false to indicate that they don't wish to be
     * unloaded, but the return value from JS components' canUnload is ignored:
     * mark-and-sweep will keep everything around until it's no longer in use,
     * making unconditional ``unload'' safe.
     *
     * You still need to provide a (likely useless) canUnload method, though:
     * it's part of the nsIModule interface contract, and the JS loader _will_
     * call it.
     */
    canUnload: function(compMgr) {
		return true;
    }
};

function NSGetModule(compMgr, fileSpec) {
    return vCoreModule;
}

