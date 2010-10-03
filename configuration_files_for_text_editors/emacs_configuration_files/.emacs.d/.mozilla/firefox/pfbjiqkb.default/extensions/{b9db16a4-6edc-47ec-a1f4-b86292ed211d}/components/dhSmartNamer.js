/******************************************************************************
 *            Copyright (c) 2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_SMARTNAMER_CID = Components.ID("{7d087a1b-d9f1-4698-92ea-b9a1b514ebde}");
const NS_SMARTNAMER_PROG_ID = "@downloadhelper.net/smart-namer;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function SmartNamer() {
	try {
		//dump("[SmartNamer] constructor\n");
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                                   .getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.smartnamer.");
        this.observerService = Components.classes["@mozilla.org/observer-service;1"]
                    .getService(Components.interfaces.nsIObserverService);
        this.observerService.addObserver(this,"quit-application",false);
		this.file = Components.classes["@mozilla.org/file/directory_service;1"]
    		.getService(Components.interfaces.nsIProperties)
        	.get("ProfD", Components.interfaces.nsIFile);
	    this.file.append("dh-smart-names.rdf");
		if(!this.file.exists()) {
			var IOS= Components.classes["@mozilla.org/network/io-service;1"]
			                    	    .getService(Components.interfaces.nsIIOService);
			var channel=IOS.newChannel("chrome://dwhelper/content/smartname.rdf",null,null);
			var input=channel.open();
			var bstream = Components.classes["@mozilla.org/binaryinputstream;1"].
				createInstance(Components.interfaces.nsIBinaryInputStream);
			bstream.setInputStream(input);
			var bytes = bstream.readBytes(bstream.available());
			input.close();
			var stream = Components.classes["@mozilla.org/network/safe-file-output-stream;1"].
				createInstance(Components.interfaces.nsIFileOutputStream);
			stream.init(this.file, 0x04 | 0x08 | 0x20, 0644, 0); // write, create, truncate
			stream.write(bytes, bytes.length);
			if (stream instanceof Components.interfaces.nsISafeOutputStream) {
				stream.finish();
			} else {
				stream.close();
			}
		}
		this.datasource=Util.getDatasourceFromRDFFile(this.file);
	} catch(e) {
		dump("[SmartNamer] !!! constructor: "+e+"\n");
	}
}

SmartNamer.prototype = {
		get enabled() { return this.pref.getBoolPref("enabled"); }
}

SmartNamer.prototype.updateEntry=function(entry) {
	
	//dump("[SmartNamer] updateEntry()\n");
	if(!this.enabled)
		return;
	var method=Util.getPropsString(entry,"capture-method");
	if(method!="network" && method!="youtube" && method!="youtube-hq" && method!="medialink")
		return;
	var pageUrl=Util.getPropsString(entry,"page-url");
	if(pageUrl==null)
		return;
	var docs=[];
	try {
		var domain=/^https?:\/\/([^\/]+)/.exec(pageUrl)[1];
		var domains=[];
		var parts=domain.split(".");
		for(var i=0;i<parts.length-1;i++) {
			var subdomain=parts.slice(i,parts.length).join(".");
			domains.push(subdomain);
		}

		var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]  
		                            .getService(Components.interfaces.nsIWindowMediator);  
		var enumerator = wm.getEnumerator("navigator:browser");  
		while(enumerator.hasMoreElements()) {  
			var win = enumerator.getNext();  
		    var tabbrowser = win.getBrowser();
		    var numTabs = tabbrowser.browsers.length;
		    for(var index=0; index<numTabs; index++) {
				var currentBrowser = tabbrowser.getBrowserAtIndex(index);
				var bUrl=currentBrowser.contentDocument.URL;
				if(bUrl==pageUrl) {
				    docs.push({
				  	  document: currentBrowser.contentDocument,
				  	  url: bUrl
				    });
				}
		    }
		}  
	
		var found={}
		var modes=["name","descr"];
		var RDF=Util.getRDF();
		for(var i in docs) {
			var document=docs[i].document;
			for(var j in modes) {
				var mode=modes[j];
				if(found[mode]==null) {
					for(var k in domains) {
						var domain=domains[k];
						var l=this.datasource.GetSources(
								RDF.GetResource(DHNS+"domain"),
								RDF.GetLiteral(domain),
								true
								);
						while(l.hasMoreElements()) {
							var source=l.getNext().QueryInterface(Components.interfaces.nsIRDFNode);
							var srcMode=Util.getPropertyValueRS(this.datasource,source,DHNS+"mode");
							if(srcMode==mode) {
								Util.setPropsString(entry,"sn-domain",domain);
								var xpath=Util.getPropertyValueRS(this.datasource,source,DHNS+"xpath");
								var text=Util.xpGetString(document.documentElement,xpath);
								if(text && text.length>0) {
									text=text.replace(/\s{1,}/g,' ');
									text=text.replace(/^\s+/,'');
									text=text.replace(/\s+$/,'');
									var regexp=Util.getPropertyValueRS(this.datasource,source,DHNS+"regexp");
									if(regexp.length>0) {
										var m=null;
										try {
											m=new RegExp(regexp).exec(text);
											if(m==null || m.length==0)
												text="";
											else if(m.length==1)
												text=m[0];
											else if(m.length>1)
												text=m[1];
											text=text.replace(/^\s+/,'');
											text=text.replace(/\s+$/,'');
										} catch(e) {}
									}
									if(text.length>0) {
										found[mode]=text;
										this.incrStat(source,"xpfound");
										if(mode=="name") {
											if(Util.getPropsString(entry,"file-name") && Util.getPropsString(entry,"sn-has-org-filename")!="yes") {
												Util.setPropsString(entry,"sn-has-org-filename","yes");
												Util.setPropsString(entry,"sn-org-filename",Util.getPropsString(entry,"file-name"));
											}
											if(this.pref.getBoolPref("fname.keep-spaces")==false) {
												text=text.replace(/\s+/g,'_');
											}
											if(this.pref.getBoolPref("fname.keep-nonascii")==false) {
												var fname="";
												for(var n=0;n<text.length;n++) {
													if(text.charCodeAt(n)<128)
														fname=fname+text.charAt(n);
												}
												text=fname;
											}
											var fname="";
											for(var n=0;n<text.length;n++) {
												if(text.charCodeAt(n)>=32)
													fname=fname+text.charAt(n);
											}
											text=fname;
											text=text.replace(/[\/:!\*\?&\|"'\\]/g,'_');
											var maxlength=this.pref.getIntPref("fname.maxlen");
											var extension=Util.getPropsString(entry,"file-extension");
											if(extension) {
												if(text.length+extension.length+1>maxlength)
													text=text.substr(0,maxlength-extension.length-1);
//												text=text.substr(-(maxlength-extension.length-1),maxlength);
												fname=text+"."+extension;
											} else {
												if(text.length>maxlength)
													text=text.substr(0,maxlength);
												fname=text;
											}
											Util.setPropsString(entry,"file-name",fname);
											if(Util.getPropsString(entry,"sn-preserve-label")!="yes") {
												var label=fname;
												if(entry.has("label-prefix")) {
													label=Util.getPropsString(entry,"label-prefix")+label;
												}
												Util.setPropsString(entry,"label",label);
											}
										}
										break;
									}
								} else {
									if(Util.getPropsString(entry,"sn-has-org-filename")=="yes") {
										var orgName=Util.getPropsString(entry,"sn-org-filename");
										Util.setPropsString(entry,"file-name",orgName);
										Util.setPropsString(entry,"label",orgName);
									}
									this.incrStat(source,"xpnfound");
								}
							}
						}
						if(found[mode])
							break;
					}
				}
			}
		}
		for(var i in found) {
			var text=found[i];
			try {
				var maxLength=this.pref.getIntPref("max."+i);
				if(maxLength>0 && text.length>maxLength) {
					text=text.substr(0,maxLength);
				}
			} catch(e) {}
			Util.setPropsString(entry,"sn-"+i,text);
		}
	} catch(e) {
		dump("!!! [SmartNamer] updateEntry(): "+e+"\n");
	}
}

SmartNamer.prototype.incrStat=function(node,stat) {
	var statValue=Util.getPropertyValueRS(this.datasource,node,DHNS+"stat-"+stat);
	if(statValue && parseInt(statValue)!=NaN)
		statValue=parseInt(statValue);
	else
		statValue=0;
	statValue++;
	Util.setPropertyValueRS(this.datasource,node,DHNS+"stat-"+stat,statValue);
	this.mayShare();
}

SmartNamer.prototype.incrNameStat=function(entry,stat) {
	//dump("[SmartNamer] incrNameStat(entry,"+stat+")\n");
	if(!this.enabled)
		return;
	var domain=Util.getPropsString(entry,"sn-domain");
	if(domain==null) {
		return;
	}
	var RDF=Util.getRDF();
	var i=this.datasource.GetSources(
			RDF.GetResource(DHNS+"domain"),
			RDF.GetLiteral(domain),
			true
			);
	while(i.hasMoreElements()) {
		var node=i.getNext().QueryInterface(Components.interfaces.nsIRDFNode);
		var mode=Util.getPropertyValueRS(this.datasource,node,DHNS+"mode");
		if(mode=="name") {
			this.incrStat(node,stat);
			return;
		}
	}
}

SmartNamer.prototype.canHandle=function(document,window,item) {
	//dump("[SmartNamer] canHandle("+document.URL+",window,"+item.tagName+")\n");
	switch(item.id) {
	case "dwhelper-snmenu":
		return this.enabled;
	case "dwhelper-snmenu-set-name":
	case "dwhelper-snmenu-set-descr":
		var range=this.getSelectionRange(window);
		return (range!=null);
	case "dwhelper-snmenu-share-now":
		return this.pref.getBoolPref("manual-share");
	case "dwhelper-snmenu-import":
		return true;
	}
	return false;
}

SmartNamer.prototype.handle=function(document,window,item) {
	//dump("[SmartNamer] handle("+document.URL+",window,"+item.tagName+")\n");
	try {
		switch(item.id) {
		case "dwhelper-snmenu-set-name":
		case "dwhelper-snmenu-set-descr":
			var range=this.getSelectionRange(window);
			if(range) {
				var dialogData={
					node: range.commonAncestorContainer
				}
				if(item.id=="dwhelper-snmenu-set-descr")
					dialogData.mode="descr";
				else
					dialogData.mode="name";
		        var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
		                                    .getService(Components.interfaces.nsIWindowMediator);
				var w = wm.getMostRecentWindow("navigator:browser");
				w.openDialog('chrome://dwhelper/content/smart-name-define.xul','dwhelper-dialog',"chrome,centerscreen,modal",dialogData);
				if(dialogData.ok) {
					var domain=Util.getPropsString(dialogData.value,"domain");
					var mode=Util.getPropsString(dialogData.value,"mode");
					var rdfNode=this.getEntry(domain,mode);
					this.datasource.beginUpdateBatch();
					if(rdfNode==null)
						rdfNode=Util.createAnonymousNodeS(this.datasource,"urn:root");
					Util.setPropertyValueRS(this.datasource,rdfNode,DHNS+"mode",dialogData.mode);
					var keys=dialogData.value.getKeys({});
					for(var i in keys) {
						var keyValue=Util.getPropsString(dialogData.value,keys[i]);
						Util.setPropertyValueRS(this.datasource,rdfNode,DHNS+keys[i],keyValue);
					}
					Util.setPropertyValueRS(this.datasource,rdfNode,DHNS+"local","1");
					this.datasource.endUpdateBatch();
					this.mayShare();
					this.saveDatasource();
					this.updateAll();
				}
			}
			break;
		case "dwhelper-snmenu-share-now":
			this.share();
			break;
		case "dwhelper-snmenu-import":
			this.import();
			break;
		}	
	} catch(e) {
		dump("!!! [SmartNamer] handle("+document.URL+",window,"+item.tagName+"): "+e+"\n");
	}
}

SmartNamer.prototype.getSelectionRange=function(window) {
	if(window.document.popupNode==null) 
		return null;
	var seln=window.document.popupNode.ownerDocument.defaultView.getSelection();
	if(seln.rangeCount>0) {
		var range=seln.getRangeAt(0);
		if(!range.collapsed)
			return range;		
	}
	return null;
}

SmartNamer.prototype.removeEntry=function(rdfNode) {
	Util.removeReference(this.datasource,rdfNode);
	this.mayShare();
	this.saveDatasource();
	this.updateAll();
}

SmartNamer.prototype.share=function() {
	//dump("[SmartNamer] share()\n");
	var props=["domain","xpath","regexp","mode","stat-xpfound","stat-xpnfound","stat-keep","stat-nkeep"];
	var xml=[];
	var haveStats=false;
	xml.push("<?xml version='1.0'?>\n<smartnamer>\n");
	var nodes=Util.getChildResourcesS(this.datasource,"urn:root",{});
	for(var i in nodes) {
		var node=nodes[i];
		//dump("----\n");
		var xml1=[];
		var haveEntryStats=false;
		xml1.push("  <entry");
		for(var j in props) {
			var prop=props[j];
			var value=Util.getPropertyValueRS(this.datasource,node,DHNS+prop);
			if(value) {
				xml1.push(" "+prop+"='"+Util.xmlEscape(value)+"'");
				//dump(prop+": "+value+"\n");
				if(prop.substr(0,5)=="stat-") {
					if(value!="0") {
						haveEntryStats=true;
					}
					Util.setPropertyValueRS(this.datasource,node,DHNS+prop,"0");
				}
			}
		}
		xml1.push("/>\n");
		if(haveEntryStats) {
			xml=xml.concat(xml1);
			haveStats=true;
		}
	}
	xml.push("</smartnamer>\n");
	if(haveStats) {
		try {
		    var xmlhttp = Components.classes["@mozilla.org/xmlextras/xmlhttprequest;1"].
		    	createInstance(Components.interfaces.nsIXMLHttpRequest);
		    xmlhttp.open ("POST", "http://www.downloadhelper.net/share-smartname.php")
		    xmlhttp.send(xml.join(""));
		} catch(e) {
			dump("!!! [SmartNamer] share(): "+e+"\n");
		}
	}
}

SmartNamer.prototype.import=function() {
	//dump("[SmartNamer] import()\n");
	function StreamListener(service) {
		this.service=service;
	}

	StreamListener.prototype={
		QueryInterface: function(iid) {
		    if (iid.equals(Components.interfaces.nsISupports) || 
		    	iid.equals(Components.interfaces.nsIInterfaceRequestor) ||
		    	iid.equals(Components.interfaces.nsIStreamListener)) {
		    	return this;
		    }
	        throw Components.results.NS_ERROR_NO_INTERFACE;
		},
		onStartRequest: function(request,context) {
			this.data="";
		},
		onDataAvailable: function(request,context,inputStream,offset,count) {
			var sstream = Components.classes["@mozilla.org/intl/converter-input-stream;1"]
	               .createInstance(Components.interfaces.nsIConverterInputStream);
			sstream.init(inputStream, "utf-8", 256, 
				Components.interfaces.nsIConverterInputStream.DEFAULT_REPLACEMENT_CHARACTER);

			var str={};
			var n=sstream.readString(128,str);
			while(n>0) {
				this.data+=str.value;
				str={};
				n=sstream.readString(128,str);
			}
		},
		onStopRequest: function(request,context,nsresult) {
			var responseStatus=request.QueryInterface(Components.interfaces.nsIHttpChannel).responseStatus;
			if(responseStatus==200) {
				this.service.importedData(this.data);
			} else {
				dump("!!! [SmartNamer] import(): failed download "+responseStatus+"\n");
			}
		},
		getInterface: function(iid) {
		    if (iid.equals(Components.interfaces.nsISupports) || 
		    	iid.equals(Components.interfaces.nsIInterfaceRequestor) ||
		    	iid.equals(Components.interfaces.nsIStreamListener)) {
		    	return this;
		    }
		    return null;
		},
	}

	var url="http://www.downloadhelper.net/smartname-data.php";
	var IOS= Components.classes["@mozilla.org/network/io-service;1"]
	    .getService(Components.interfaces.nsIIOService);
	var uri = IOS.newURI(url, null, null);
	var channel = IOS.newChannelFromURI(uri);
	var listener = new StreamListener(this);
	channel.notificationCallbacks = listener;
	channel.asyncOpen(listener, null);
}

SmartNamer.prototype.importedData=function(data) {
	try {
		//dump("[SmartNamer] importedData():\n"+data+"------\n");
		var datasource=Components.classes
			['@mozilla.org/rdf/datasource;1?name=in-memory-datasource'].
	  		createInstance(Components.interfaces.nsIRDFDataSource);
		var parser=Components.classes
			['@mozilla.org/rdf/xml-parser;1'].
	      		createInstance(Components.interfaces.nsIRDFXMLParser);
		var uri = Components.classes["@mozilla.org/network/standard-url;1"].
	        createInstance(Components.interfaces.nsIURI);
		uri.spec = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
		parser.parseString(datasource,uri,data);
		
		var RDF=Util.getRDF();
		
		var localNodes=[];
		var changedLocalNodes=[];
		var keepNodes=[];
		var l=this.datasource.GetSources(
				RDF.GetResource(DHNS+"local"),
				RDF.GetLiteral("1"),
				true
				);
		while(l.hasMoreElements()) {
			var localNode=l.getNext().QueryInterface(Components.interfaces.nsIRDFNode);
			var domain=Util.getPropertyValueRS(this.datasource,localNode,DHNS+"domain");			
			var mode=Util.getPropertyValueRS(this.datasource,localNode,DHNS+"mode");			
			var xpath=Util.getPropertyValueRS(this.datasource,localNode,DHNS+"xpath");			
			var regexp=Util.getPropertyValueRS(this.datasource,localNode,DHNS+"regexp");			
			localNodes.push(localNode);
			var found=false;
			var m=datasource.GetSources(
					RDF.GetResource(DHNS+"domain"),
					RDF.GetLiteral(domain),
					true
					);
			while(m.hasMoreElements()) {
				var node=m.getNext().QueryInterface(Components.interfaces.nsIRDFNode);
				var mode0=Util.getPropertyValueRS(datasource,node,DHNS+"mode");
				var xpath0=Util.getPropertyValueRS(datasource,node,DHNS+"xpath");
				var regexp0=Util.getPropertyValueRS(datasource,node,DHNS+"regexp");
				if(mode==mode0) {
					found=true;
					if(xpath!=xpath0 || regexp!=regexp0) {
						changedLocalNodes.push(localNode);
					}
				}
			}
			if(found==false) {
				keepNodes.push(localNode);
			}
		}
				
		for(var i in changedLocalNodes) {
			var localNode=changedLocalNodes[i];
			var domain=Util.getPropertyValueRS(this.datasource,localNode,DHNS+"domain");			
			var mode=Util.getPropertyValueRS(this.datasource,localNode,DHNS+"mode");			
			var xpath=Util.getPropertyValueRS(this.datasource,localNode,DHNS+"xpath");			
			var regexp=Util.getPropertyValueRS(this.datasource,localNode,DHNS+"regexp");			
		}
		
		if(changedLocalNodes.length>0) {
			var dialogData={
				overlap: changedLocalNodes
			};
	        var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
	                                    .getService(Components.interfaces.nsIWindowMediator);
			var w = wm.getMostRecentWindow("navigator:browser");
			w.openDialog('chrome://dwhelper/content/smart-name-overlap.xul','dwhelper-dialog',"chrome,centerscreen,modal",dialogData);
			if(!dialogData.ok)
				return;
			keepNodes=keepNodes.concat(dialogData.value);
		}
		
		var props=["domain","mode","xpath","regexp","local","stat-xpfound","stat-xpnfound","stat-keep","stat-nkeep"];
		for(var i in keepNodes) {
			var node=keepNodes[i];
			var domain=Util.getPropertyValueRS(this.datasource,node,DHNS+"domain");			
			var mode=Util.getPropertyValueRS(this.datasource,node,DHNS+"mode");
			Util.removeReferenceS(datasource,DHNS+domain+"/"+mode);
			var node0=Util.createAnonymousNodeS(datasource,"urn:root");
			for(var j in props) {
				var prop=props[j];
				var value=Util.getPropertyValueRS(this.datasource,node,DHNS+prop);
				if(value) {
					Util.setPropertyValueRS(datasource,node0,DHNS+prop,value);
				}
			}
		}
		Util.emptyDatasource(this.datasource);
		Util.concatDatasource(this.datasource,datasource);
		this.saveDatasource();
		this.updateAll();
	} catch(e) {
		dump("!!! [SmartNamer] importedData(): "+e+"\n");
	}
}

SmartNamer.prototype.getEntry=function(domain,mode) {
	try {
	var RDF=Util.getRDF();
	var i=this.datasource.GetSources(
			RDF.GetResource(DHNS+"domain"),
			RDF.GetLiteral(domain),
			true
			);
	while(i.hasMoreElements()) {
		var source=i.getNext().QueryInterface(Components.interfaces.nsIRDFNode);
		var srcMode=Util.getPropertyValueRS(this.datasource,source,DHNS+"mode");
		if(srcMode==mode)
			return source;
	}
	return null;
	
	} catch(e) {
		dump("!!! [SmartNamer] getEntry("+domain+","+mode+"): "+e+"\n");
	}
}

SmartNamer.prototype.mayShare=function() {
	//dump("[SmartNamer] mayShare()\n");
	
	if(this.pref.getBoolPref("auto-share")==false)
		return;
	var now=new Date().getTime()/1000;
	try {
		var last=this.pref.getIntPref("last-shared");
		if(now-last>(60*60*24)) { // once a day
			this.share();
			this.pref.setIntPref("last-shared",now);
		}
	} catch(e) {
		this.pref.setIntPref("last-shared",now);
	}
}

SmartNamer.prototype.observe=function(subject,topic,data) {
    if(topic=="quit-application") {
    	this.mayShare();
    	this.saveDatasource();
    	this.observerService.removeObserver(this,"quit-application");
    }
}

SmartNamer.prototype.saveDatasource=function() {
	//dump("[SmartNamer] saveDatasource()\n");
	try {
		var serializer="@mozilla.org/rdf/xml-serializer;1";
		var s=Components.classes[serializer].createInstance(Components.interfaces.nsIRDFXMLSerializer);
		s.init(this.datasource);
		var stream = Components.classes['@mozilla.org/network/file-output-stream;1']
		    .createInstance(Components.interfaces.nsIFileOutputStream);
		stream.init(this.file, 42, 0644, 0); 
	
		s.QueryInterface(Components.interfaces.nsIRDFXMLSource).Serialize(stream);
		stream.close();
	} catch(e) {
		dump("!!! [SmartNamer] saveDatasource: "+e+"\n");
	}
}

SmartNamer.prototype.getDatasource=function() {
	//dump("[SmartNamer] getDatasource()\n");
	return this.datasource;
}

SmartNamer.prototype.updateAll=function() {
	if(this.core==null)
		this.core=Components.classes["@downloadhelper.net/core;1"].
			getService(Components.interfaces.dhICore);
	this.core.updateSmartName();
}

SmartNamer.prototype.QueryInterface = function(iid) {
	//dump("[SmartNamer] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhISmartNamer) ||
        iid.equals(Components.interfaces.nsIObserver) ||
        iid.equals(Components.interfaces.dhIContextItem) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vSmartNamerModule = {
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
        compMgr.registerFactoryLocation(NS_SMARTNAMER_CID,
                                        "SmartNamer",
                                        NS_SMARTNAMER_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_SMARTNAMER_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_SMARTNAMER_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vSmartNamerFactory;
    },

    /* factory object */
    vSmartNamerFactory: {
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

			return new SmartNamer().QueryInterface(iid);
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
    return vSmartNamerModule;
}

