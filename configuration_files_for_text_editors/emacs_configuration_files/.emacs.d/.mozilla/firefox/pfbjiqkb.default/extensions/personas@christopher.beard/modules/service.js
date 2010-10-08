/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Personas.
 *
 * The Initial Developer of the Original Code is Mozilla.
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Chris Beard <cbeard@mozilla.org>
 *   Myk Melez <myk@mozilla.org>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

let EXPORTED_SYMBOLS = ["PersonaService", "PERSONAS_EXTENSION_ID"];

const Cc = Components.classes;
const Ci = Components.interfaces;
const Cr = Components.results;
const Cu = Components.utils;

// modules that come with Firefox
Cu.import("resource://gre/modules/XPCOMUtils.jsm");
// LightweightThemeManager may not be not available (Firefox < 3.6 or Thunderbird)
try { Cu.import("resource://gre/modules/LightweightThemeManager.jsm"); }
catch (e) { LightweightThemeManager = null; }

// modules that are generic
Cu.import("resource://personas/modules/JSON.js");
Cu.import("resource://personas/modules/log4moz.js");
Cu.import("resource://personas/modules/Observers.js");
Cu.import("resource://personas/modules/Preferences.js");
Cu.import("resource://personas/modules/StringBundle.js");
Cu.import("resource://personas/modules/URI.js");

const PERSONAS_EXTENSION_ID = "personas@christopher.beard";

const COOKIE_INITIAL_PERSONA = "initial_persona";
const COOKIE_USER = "PERSONA_USER";

let PersonaService = {
  THUNDERBIRD_ID: "{3550f703-e582-4d05-9a08-453d09bdfdc6}",
  FIREFOX_ID:     "{ec8030f7-c20a-464f-9b0e-13a3a9e97384}",

  //**************************************************************************//
  // Shortcuts

  // Access to extensions.personas.* preferences.  To access other preferences,
  // call the Preferences module directly.
  get _prefs() {
    delete this._prefs;
    return this._prefs = new Preferences("extensions.personas.");
  },

  get _strings() {
    delete this._strings;
    return this._strings = new StringBundle("chrome://personas/locale/personas.properties");
  },

  get appInfo() {
    delete this.appInfo;
    return this.appInfo = Cc["@mozilla.org/xre/app-info;1"].
                           getService(Ci.nsIXULAppInfo).
                           QueryInterface(Ci.nsIXULRuntime);
  },

  get extension() {
    delete this.extension;

    if (this.appInfo.ID == this.FIREFOX_ID) {
      return this.extension = Cc["@mozilla.org/fuel/application;1"].
                              getService(Ci.fuelIApplication).
                              extensions.get(PERSONAS_EXTENSION_ID);
    }

    // If STEEL provides a FUEL-compatible extIExtension interface
    // in Thunderbird, return it here.

    return this.extension = null;
  },

  get _log() {
    delete this._log;
    return this._log = Log4Moz.getConfiguredLogger("PersonaService");
  },


  //**************************************************************************//
  // Initialization & Destruction

  _init: function() {
    // Observe quit so we can destroy ourselves.
    Observers.add("quit-application", this.onQuitApplication, this);
    // Observe the "cookie-changed" topic to load the favorite personas when
    // the user signs in.
    Observers.add("cookie-changed", this.onCookieChanged, this);
    // Observe the "lightweight-theme-changed" to sync the add-on with the
    // lightweight theme manager.
    Observers.add("lightweight-theme-changed",
                  this.onLightweightThemeChanged, this);

    this._prefs.observe("useTextColor",   this.onUseColorChanged,     this);
    this._prefs.observe("useAccentColor", this.onUseColorChanged,     this);
    this._prefs.observe("selected",       this.onSelectedModeChanged, this);

    // Get the initial persona specified by a cookie, if any.  The gallery
    // sets this when users download Personas from the Details page
    // for a specific persona, so that the user sees that persona when they
    // install the extension.  We only check for a cookie on first run,
    // because it would be expensive to traverse cookies every time, and since
    // the gallery only sets the initial persona cookie on installation.
    let personaFromCookie;
    if (this.extension && this.extension.firstRun)
      personaFromCookie = this._getPersonaFromCookie();

    // Change to the initial persona if preferences indicate that a persona
    // should be active but they don't specify the persona that is active.
    // This normally happens only on first run, although it could theoretically
    // happen at other times.
    //
    // The initial persona is either the persona specified by a cookie (set by
    // the gallery), the one specified by the extensions.personas.initial
    // preference (set by a distribution.ini file for a BYOB/bundle/distributon
    // build), or the Groovy Blue persona which is hardcoded into this code.
    //
    // NOTE: the logic here is carefully designed to achieve the correct outcome
    // under a variety of circumstances and should be changed with caution!
    // See bug 513765 and bug 503300 for some details on why it works this way.
    //
    if (this._prefs.get("selected") == "current" && !this._prefs.get("current")) {
      if (personaFromCookie)
        this.changeToPersona(personaFromCookie);
      else if (this._prefs.has("initial"))
        this.changeToPersona(JSON.parse(this._prefs.get("initial")));
      else if (LightweightThemeManager && LightweightThemeManager.currentTheme)
        this.changeToPersona(LightweightThemeManager.currentTheme);
      else {
        this.changeToPersona({
          "id":"33",
          "name":"Groovy Blue",
          "accentcolor":"#6699ff",
          "textcolor":"#07188d",
          "header":"3\/3\/33\/tbox-groovy_blue.jpg",
          "footer":"3\/3\/33\/stbar-groovy_blue.jpg",
          "category":"Abstract",
          "description":null,
          "author":"Lee.Tom",
          "username":"Lee.Tom",
          "detailURL":"http:\/\/www.getpersonas.com\/persona\/33",
          "headerURL":"http:\/\/www.getpersonas.com\/static\/3\/3\/33\/tbox-groovy_blue.jpg",
          "footerURL":"http:\/\/www.getpersonas.com\/static\/3\/3\/33\/stbar-groovy_blue.jpg",
          "previewURL":"http:\/\/www.getpersonas.com\/static\/3\/3\/33\/preview.jpg",
          "iconURL":"http:\/\/www.getpersonas.com\/static\/3\/3\/33\/preview_small.jpg",
          "dataurl":"data:image\/png;base64,\/9j\/4AAQSkZJRgABAQEASABIAAD\/2" +
                    "wBDAAEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQIC" +
                    "AQECAQEBAgICAgICAgICAQICAgICAgICAgL\/2wBDAQEBAQEBAQEBAQ" +
                    "ECAQEBAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA" +
                    "gICAgICAgICAgICAgL\/wAARCAAQABADAREAAhEBAxEB\/8QAFwAAAw" +
                    "EAAAAAAAAAAAAAAAAABAcICv\/EACAQAAIDAAEFAQEAAAAAAAAAAAME" +
                    "AQIFBgcIERIjEyX\/xAAZAQACAwEAAAAAAAAAAAAAAAADCAQFBwn\/x" +
                    "AAlEQACAQMCBgMBAAAAAAAAAAABAgMEERIAIQUGExQiMQcyM1L\/2gA" +
                    "MAwEAAhEDEQA\/AL9p21L52peGctNVXkGbWsGZhNZlXBWo0sHVSVucA" +
                    "9BryNibVm9Iq3ahWLengV+g03yTMyl+6aV1YE45YmTZsGsCVxBUjx+p" +
                    "sovuFgi5IjlgRUhI7RjYHyJy93Y5EKxBW+9wrKN9wjsft65bGnnPK4+" +
                    "kTguZqqKcdJstEzbuzyTdErUtSFqszqY4uR7lQGdQ+q13rML1vWIrN1" +
                    "U\/I1N2FTG6oKipx6pUlijxRM+ICsRE8scbuqSDEqCvsAiRJyNRvUBH" +
                    "qcq9I5XdFTxaNbKcmxdWMcYRSt\/IqokK5a0NdQOicaI0cEnGi6eWiW" +
                    "TKvqghltgpT1GECpZj+DYQmXSG97SAtxBkXibk9Ul4TzXMk3dSV6wMg" +
                    "a6MSDshAuB+xYiNQVs4F8jZV1u9RwmCKGdKWneSorMFJGPTQGRWkYlj" +
                    "ZVAyKqQRckAG+i+M9rHGtTUI31CU3dwGWRG+GN7lTWmhe4VpCxNRnr+" +
                    "y47Wm9qxSwigg5A0YJT6yDifOtS0MS8Km6JmDdZelgASbgeyrEfUtdg" +
                    "4VXKoSU0bhlDJStUxz00KgWWORAuboP6IAIBIywKgKSRd7Btf\/2Q=="
        });
      }
    }

    let timerManager = Cc["@mozilla.org/updates/timer-manager;1"].
                       getService(Ci.nsIUpdateTimerManager);

    // Load cached personas data
    this.loadDataFromCache();

    // Refresh data, then set a timer to refresh it periodically.
    // This isn't quite right, since we always load data on startup, even if
    // we've recently refreshed it.  And the timer that refreshes data ignores
    // the data load on startup, so if it's been more than the timer interval
    // since a user last started her browser, we load the data twice:
    // once because the browser starts and once because the refresh timer fires.
    this.refreshData();
    let dataRefreshCallback = {
      _svc: this,
      notify: function(timer) { this._svc._refreshDataWithMetrics() }
    };
    timerManager.registerTimer("personas-data-refresh-timer",
                               dataRefreshCallback,
                               86400 /* in seconds == one day */);

    // Refresh the current persona once per day.  We only do this for
    // Thunderbird and Firefox < 3.6, since the LightweightThemeManager
    // does this for Firefox 3.6.
    if (!LightweightThemeManager) {
      let personaRefreshCallback = {
        _svc: this,
        notify: function(timer) { this._svc._refreshPersona() }
      };
      timerManager.registerTimer("personas-persona-refresh-timer",
                                 personaRefreshCallback,
                                 86400 /* in seconds == one day */);
    }

    // Load cached favorite personas
    this.loadFavoritesFromCache();

    this.refreshFavorites();
    // Refresh the favorite personas once per day.
    let favoritesRefreshCallback = {
      _svc: this,
      notify: function(timer) { this._svc.refreshFavorites() }
    };
    timerManager.registerTimer("personas-favorites-refresh-timer",
                               favoritesRefreshCallback,
                               86400 /* in seconds == one day */);

    this._rotationTimer = Cc["@mozilla.org/timer;1"].createInstance(Ci.nsITimer);
    this.onSelectedModeChanged();
  },

  _destroy: function() {
    Observers.remove("cookie-changed", this.onCookieChanged, this);
    Observers.remove("lightweight-theme-changed",
                     this.onLightweightThemeChanged, this);

    this._prefs.ignore("useTextColor",   this.onUseColorChanged,     this);
    this._prefs.ignore("useAccentColor", this.onUseColorChanged,     this);
    this._prefs.ignore("selected",       this.onSelectedModeChanged, this);
  },


  //**************************************************************************//
  // XPCOM Plumbing

  QueryInterface: XPCOMUtils.generateQI([Ci.nsIObserver,
                                         Ci.nsIDOMEventListener,
                                         Ci.nsITimerCallback]),


  //**************************************************************************//
  // Data Retrieval

  _makeRequest: function(url, loadCallback, headers, aIsBinary) {
    let request = Cc["@mozilla.org/xmlextras/xmlhttprequest;1"].createInstance();

    request = request.QueryInterface(Ci.nsIDOMEventTarget);
    request.addEventListener("load", loadCallback, false);

    request = request.QueryInterface(Ci.nsIXMLHttpRequest);
    request.open("GET", url, true);

    // Force the request to include cookies even though this chrome code
    // is seen as a third-party, so the server knows the user for which we are
    // requesting favorites (or anything else user-specific in the future).
    // This only works in Firefox 3.6; in Firefox 3.5 the request will instead
    // fail to send cookies if the user has disabled third-party cookies.
    try {
      request.channel.QueryInterface(Ci.nsIHttpChannelInternal).
        forceAllowThirdPartyCookie = true;
    }
    catch(ex) { /* user is using Firefox 3.5 */ }

    if (headers)
      for (let header in headers)
        request.setRequestHeader(header, headers[header]);

    if (aIsBinary)
      request.overrideMimeType('text/plain; charset=x-user-defined');

    request.send(null);
  },

  /**
   * Refresh data. This method gets called on demand (including on startup)
   * and retrieves data without passing any additional information about
   * the selected persona and the application (that information is only included
   * in the daily retrieval so we can get consistent daily statistics from it
   * no matter how many times a user starts the application in a given day).
   */
  refreshData: function() {
    let url = this.dataURL + "index_" + this._prefs.get("data.version") + ".json";
    let t = this;
    this._makeRequest(url, function(evt) { t.onDataLoadComplete(evt) });
  },

  /**
   * Refresh data, providing metrics on persona usage in the process.
   * This method gets called approximately once per day on a cross-session timer
   * (provided Firefox is run every day), updates the version of the data
   * that is currently in memory, and passes information about the selected
   * persona and the host application to the server for statistical analysis
   * (f.e. figuring out which personas are the most popular).
   */
  _refreshDataWithMetrics: function() {
    let appInfo     = Cc["@mozilla.org/xre/app-info;1"].
                      getService(Ci.nsIXULAppInfo);
    let xulRuntime  = Cc["@mozilla.org/xre/app-info;1"].
                      getService(Ci.nsIXULRuntime);

    // Calculate the amount of time (in hours) since the persona was last changed.
    let duration = "";
    if (this._prefs.has("persona.lastChanged"))
      duration = Math.round((new Date() - new Date(parseInt(this._prefs.get("persona.lastChanged")))) / 1000 / 60 / 60);

    // This logic is based on ExtensionManager::_updateLocale.
    let locale;
    try {
      if (Preferences.get("intl.locale.matchOS")) {
        let localeSvc = Cc["@mozilla.org/intl/nslocaleservice;1"].
                        getService(Ci.nsILocaleService);
        locale = localeSvc.getLocaleComponentForUserAgent();
      }
      else
        throw "set locale in the catch block";
    }
    catch (ex) {
      locale = Preferences.get("general.useragent.locale");
    }

    let params = {
      type:       this.selected,
      id:         this.currentPersona ? this.currentPersona.id : "",
      duration:   duration,
      appID:      appInfo.ID,
      appVersion: appInfo.version,
      appLocale:  locale,
      appOS:      xulRuntime.OS,
      appABI:     xulRuntime.XPCOMABI
    };

    //dump("params: " + [name + "=" + encodeURIComponent(params[name]) for (name in params)].join("&") + "\n");

    let url = this.dataURL + "index_" + this._prefs.get("data.version") + ".json?" +
              [name + "=" + encodeURIComponent(params[name]) for (name in params)].join("&");
    let t = this;
    this._makeRequest(url, function(evt) { t.onDataLoadComplete(evt) });
  },

  onDataLoadComplete: function(aEvent) {
    let request = aEvent.target;

    // XXX Try to reload again sooner?
    if (request.status != 200)
      throw("problem loading data: " + request.status + " - " + request.statusText);

    this.personas = JSON.parse(request.responseText);

    // Cache the response
    let cacheDirectory =
      FileUtils.getDirectory(FileUtils.getPersonasDirectory(), "cache");
    FileUtils.writeFile(cacheDirectory, "personas.json", request.responseText);
  },

  /**
   * Attempts to load this.personas from the cached file in cache/personas.json
   */
  loadDataFromCache : function() {
    let cacheDirectory =
      FileUtils.getDirectory(FileUtils.getPersonasDirectory(), "cache");
    let data = FileUtils.readFile(cacheDirectory, "personas.json");

    try { this.personas = JSON.parse(data); }
    catch (e) {
      // Could not load from cached data, file empty or does not exist perhaps
      return;
    }

    // Now that we have data, pick a new random persona.  Currently, this is
    // the only time we pick a random persona besides when the user selects
    // the "Random From [category]" menuitem, which means the user gets a new
    // random persona each time they start the browser.
    if (this.selected == "random") {
      this.currentPersona = this._getRandomPersonaFromCategory(this.category);
      this._prefs.reset("persona.lastRefreshed");
      this._notifyPersonaChanged(this.currentPersona);
    }
  },

  /**
   * Makes a request to obtain the favorite personas json. This occurs only if
   * a user is currenly signed in.
   */
  refreshFavorites : function() {
    // Only refresh if the user is signed in at the moment.
    if (this.isUserSignedIn) {
      let url =
        "http://" + this._prefs.get("host") + "/gallery/All/Favorites?json=1";
      let t = this;
      this._makeRequest(url, function(evt) { t.onFavoritesLoadComplete(evt) });
    }
  },

  /**
   * Handles the response from the refreshFavorites method. Loads the favorite
   * personas list.
   * @param aEvent The Http request event object.
   */
  onFavoritesLoadComplete : function(aEvent) {
    let request = aEvent.target;

    if (request.status != 200)
      throw("problem loading favorites: " + request.status + " - " + request.statusText);

    try {
      this.favorites = JSON.parse(request.responseText);
    }
    catch(ex) {
      Cu.reportError("error parsing favorites data; perhaps you are using " +
                     "Firefox 3.5 and have disabled third-party cookies?");
      return;
    }

    // Cache the response
    let cacheDirectory =
      FileUtils.getDirectory(FileUtils.getPersonasDirectory(), "cache");
    FileUtils.writeFile(cacheDirectory, "favorites.json", request.responseText);
  },

  /**
   * Attempts to load this.favorites from the cached file in
   * cache/favorites.json
   */
  loadFavoritesFromCache : function() {
    let cacheDirectory =
      FileUtils.getDirectory(FileUtils.getPersonasDirectory(), "cache");
    let data = FileUtils.readFile(cacheDirectory, "favorites.json");

    try { this.favorites = JSON.parse(data); }
    catch (e) {
      // Could not load from cached data, file empty or does not exist perhaps
    }
  },

  /**
   * Adds the given persona to the favorites list. If the persona is already
   * in the list then it is replaced.
   * @param aPersona The persona object to be added.
   */
  addFavoritePersona : function(aPersona) {
    // Make sure the favorites list exists.
    if (!this.favorites)
      this.favorites = [];

    let i = this._findPersonaInArray(aPersona, this.favorites);
    if (i >= 0)
      this.favorites[i] = aPersona;
    else
      this.favorites.push(aPersona);
  },

  /**
   * Removes the given persona from the favorites list, if found.
   * @param aPersona The persona object to be removed.
   */
  removeFavoritePersona : function(aPersona) {
    // Abort if the favorites list hasn't been created.
    if (!this.favorites)
      return;

    let i = this._findPersonaInArray(aPersona, this.favorites);
    if (i >= 0)
      this.favorites.splice(i, 1);
  },

  _refreshPersona: function() {
    // Only refresh the persona if the user selected a specific persona with an
    // ID and update URL.  If the user selected a random persona, we'll change
    // it the next time we refresh the directory; if the user selected
    // the default persona, we don't need to refresh it, as it doesn't change;
    // if the user selected a custom persona (which doesn't have an ID), it's
    // not clear what refreshing it would mean; and if the persona doesn't have
    // an update URL, then we don't have a way to refresh it.
    if (this.selected != "current" ||
        !this.currentPersona ||
        !this.currentPersona.id ||
        !this.currentPersona.updateURL)
      return;

    let headers = {};

    if (this._prefs.has("persona.lastRefreshed")) {
      let date = new Date(parseInt(this._prefs.get("persona.lastRefreshed")));
      headers["If-Modified-Since"] = DateUtils.toRFC1123(date);
    }

    let currentPersona;
    try       { currentPersona = JSON.stringify(this.currentPersona) }
    catch(ex) { currentPersona = "error JSON.stringify-ing currentPersona " +
                                 this.currentPersona + ": " + ex }
    this._log.debug("_refreshPersona: currentPersona = " + currentPersona +
                    "; url = " + this.currentPersona.updateURL);

    let t = this;
    this._makeRequest(this.currentPersona.updateURL,
                      function(evt) { t.onPersonaLoadComplete(evt) },
                      headers);
  },

  onPersonaLoadComplete: function(event) {
    let request = event.target;

    this._log.debug("onPersonaLoadComplete: status = " + request.status +
                    "; responseText = " + request.responseText);

    // 304 means the file we requested has not been modified since the
    // If-Modified-Since date we specified, so there's nothing to do.
    if (request.status == 304) {
      //dump("304 - the persona has not been modified\n");
      return;
    }

    if (request.status != 200)
      throw("problem refreshing persona: " + request.status + " - " + request.statusText);

    let persona = JSON.parse(request.responseText);

    // If the persona we're refreshing is no longer the selected persona,
    // then cancel the refresh (otherwise we'd undo whatever changes the user
    // has just made).
    if (this.selected != "current" || !this.currentPersona ||
        this.currentPersona.id != persona.id) {
      //dump("persona " + persona.id + "(" + persona.name + ") no longer the current persona; ignoring refresh\n");
      return;
    }

    // If the version strings are identical, the persona hasn't changed.
    if ((persona.version || "") == (this.currentPersona.version || ""))
      return;

    // Set the current persona to the updated version we got from the server,
    // and notify observers about the change.
    this.currentPersona = persona;
    this._notifyPersonaChanged(this.currentPersona);

    // Record when this refresh took place so the next refresh only looks
    // for changes since this refresh.
    // Note: we set the preference to a string value because preferences
    // can't hold large enough integer values.
    this._prefs.set("persona.lastRefreshed", new Date().getTime().toString());
  },


  //**************************************************************************//
  // Implementation

  // The JSON feed of personas retrieved from the server.
  // Loaded upon service initialization and reloaded periodically thereafter.
  personas: null,

  // The JSON feed of favorite personas retrieved from the server.
  favorites: null,

  /**
   * extensions.personas.selected: the type of persona that the user selected;
   * possible values are default (the default Firefox theme), random (a random
   * persona from a category), current (the value of this.currentPersona), and
   * randomFavorite (a random persona from the favorite list).
   */
  get selected()        { return this._prefs.get("selected") },
  set selected(newVal)  {        this._prefs.set("selected", newVal) },

  /**
   * extensions.personas.current: the current persona
   */
  get currentPersona() {
    let current = this._prefs.get("current");
    if (current) {
      try       { return JSON.parse(current) }
      catch(ex) { Cu.reportError("error getting current persona: " + ex) }
    }
    return null;
  },
  set currentPersona(newVal) {
    try {
      this._prefs.set("current", JSON.stringify(newVal));
      this._cachePersonaImages(newVal);
    }
    catch(ex) { Cu.reportError("error setting current persona: " + ex) }
  },

  /**
   * extensions.personas.category: the category from which to pick a random
   * persona.
   */
  get category()        { return this._prefs.get("category") },
  set category(newVal)  {        this._prefs.set("category", newVal) },

  /**
   * The URL at which the static data is located.
   */
  get dataURL() {
    return "http://" + this._prefs.get("host") + "/static/";
  },

  /**
   * extensions.personas.custom: the custom persona.
   */
  get customPersona() {
    let custom = this._prefs.get("custom");
    if (custom) {
      try       { return JSON.parse(custom) }
      catch(ex) { Cu.reportError("error getting custom persona: " + ex) }
    }
    return null;
  },
  set customPersona(newVal) {
    try       { this._prefs.set("custom", JSON.stringify(newVal)) }
    catch(ex) { Cu.reportError("error setting custom persona: " + ex) }
  },

  /**
   * Notifies the persona changes or uses the lightweight theme manager
   * functionality for this purpose (if available)
   * @param aPersona the persona to be set as current if the lightweight theme
   * manager is available
   */
  _notifyPersonaChanged : function(aPersona) {
    this._log.debug("_notifyPersonaChanged:\n" + Log4Moz.getStackTrace());
    if (LightweightThemeManager)
      LightweightThemeManager.currentTheme = aPersona;
    else
      Observers.notify("personas:persona:changed");
  },

  changeToDefaultPersona: function() {
    this.selected = "default";
    this._prefs.set("persona.lastChanged", new Date().getTime().toString());
    this._notifyPersonaChanged(null);
  },

  changeToRandomPersona: function(category) {
    this.category = category;
    this.currentPersona = this._getRandomPersonaFromCategory(category);
    this.selected = "random";
    this._prefs.set("persona.lastChanged", new Date().getTime().toString());
    this._notifyPersonaChanged(this.currentPersona);
  },

  changeToRandomFavoritePersona : function() {
    if (this.favorites && this.favorites.length > 0 && this.isUserSignedIn) {
      this.currentPersona = this._getRandomPersonaFromArray(this.favorites);
      this.selected = "randomFavorite";
      this._prefs.set("persona.lastChanged", new Date().getTime().toString());
      this._notifyPersonaChanged(this.currentPersona);
    }
  },

  changeToPersona: function(persona) {
    // Check whether the persona is in the favorites or the recent lists,
    // in which case the change-notification should not be shown.
    let recent = this.getRecentPersonas();
    let favorites = this.favorites;
    let inRecent =
      (recent && recent.some(function(v) v.id == persona.id));
    let inFavorites =
      (favorites && favorites.some(function(v) v.id == persona.id));

    this.currentPersona = persona;
    this._addPersonaToRecent(persona);
    this.selected = "current";
    this._prefs.reset("persona.lastRefreshed");
    this._prefs.set("persona.lastChanged", new Date().getTime().toString());
    this._notifyPersonaChanged(this.currentPersona);

    // Show the notification if the selected persona is not in the favorite or
    // recent lists, is not a custom persona and its author or username is not null.
    // In this case we make sure at least one of these two fields is not null
    // to prevent bug 526788.
    if (!inRecent && !inFavorites && !persona.custom && (persona.author || persona.username))
      this._showPersonaChangeNotification();
  },

  /**
   * Reverts the current persona to the previously selected persona, if
   * available
   */
  revertToPreviousPersona : function() {
    let undonePersonaId = this.currentPersona.id;
    let previousPersona = this._prefs.get("lastselected1");
    if (previousPersona) {
      this.currentPersona = JSON.parse(previousPersona);
      this._revertRecent();
      this.selected = "current";

      if (LightweightThemeManager) {
        // forget the lightweight theme too
        LightweightThemeManager.forgetUsedTheme(undonePersonaId);
        LightweightThemeManager.currentTheme = this.currentPersona;
      }
      else
        this.resetPersona();
    }
  },

  /**
   * Shows a notification displaying the currently selected persona and button
   * to revert the changes.
   */
  _showPersonaChangeNotification : function() {
    // Obtain most recent window and its notification box
    let wm =
      Cc["@mozilla.org/appshell/window-mediator;1"].
        getService(Ci.nsIWindowMediator);

    let notificationBox;
    switch (this.appInfo.ID) {
      case this.FIREFOX_ID:
        notificationBox = wm.getMostRecentWindow("navigator:browser").
                          getBrowser().getNotificationBox();
        break;
      case this.THUNDERBIRD_ID:
        notificationBox = wm.getMostRecentWindow("mail:3pane").
                          document.getElementById("mail-notification-box");
        break;
      default:
        throw "unknown application ID " + this.appInfo.ID;
    }

    // If there is another notification of the same kind already, remove it.
    let oldNotification =
      notificationBox.getNotificationWithValue("lwtheme-install-notification");
    if (oldNotification)
      notificationBox.removeNotification(oldNotification);

    let message = this._strings.get("notification.personaWasSelected",
                                    [this.currentPersona.name,
                                     (this.currentPersona.author ?
                                      this.currentPersona.author :
                                      this.currentPersona.username)]);

    let revertButton = {
      label     : this._strings.get("notification.revertButton.label"),
      accesskey : this._strings.get("notification.revertButton.accesskey"),
      popup     : null,
      callback  : function() { PersonaService.revertToPreviousPersona(); }
    };

    notificationBox.appendNotification(
      message, "lwtheme-install-notification", null,
      notificationBox.PRIORITY_INFO_MEDIUM, [ revertButton ] );
  },

  /**
   * Looks for the given persona in the given array and returns its index.
   * @param aPersona The persona to be found.
   * @param aPersonaArray The array in which to look for the persona.
   * @return The index of the persona in the array; -1 if not found.
   */
  _findPersonaInArray : function(aPersona, aPersonaArray) {
    for (let i = 0; i < aPersonaArray.length; i++) {
      if (aPersonaArray[i].id == aPersona.id)
        return i;
    }
    return -1;
  },

  _getRandomPersonaFromArray : function(aPersonaArray) {
    // Get a random item from the list, trying up to five times to get one
    // that is different from the currently-selected item in the category
    // (if any).  We use Math.floor instead of Math.round to pick a random
    // number because the JS reference says Math.round returns a non-uniform
    // distribution
    // <http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Global_Objects:Math:random#Examples>.
    if (aPersonaArray && aPersonaArray.length > 0) {
      let randomIndex, randomItem;
      for (let i = 0; i < 5; i++) {
        randomIndex = Math.floor(Math.random() * aPersonaArray.length);
        randomItem = aPersonaArray[randomIndex];
        if (!this.currentPersona || randomItem.id != this.currentPersona.id)
          break;
      }

      return randomItem;
    }
    return this.currentPersona;
  },

  _getRandomPersonaFromCategory: function(aCategoryName) {
    // If we have the list of categories, use it to pick a random persona
    // from the selected category.
    if (this.personas && this.personas.categories) {
      let personas;
      for each (let category in this.personas.categories) {
        if (aCategoryName == category.name) {
          personas = category.personas;
          break;
        }
      }

      return this._getRandomPersonaFromArray(personas);
    }
    return this.currentPersona;
  },

  /**
   * Obtains the list of recently selected personas, parsed from the
   * "lastselected" preferences.
   * @param aHowMany (Optional, default 4) How many personas to obtain.
   * @return The list of recent personas.
   */
  getRecentPersonas : function(aHowMany) {
    if (!aHowMany)
      aHowMany = 4;

    // Parse the list from the preferences
    let personas = [];
    for (let i = 0; i < aHowMany; i++) {
      if (this._prefs.has("lastselected" + i)) {
        try {
          personas.push(JSON.parse(this._prefs.get("lastselected" + i)));
        }
        catch(ex) {}
      }
    }

    return personas;
  },

  _addPersonaToRecent: function(persona) {
    let personas = this.getRecentPersonas();

    // Remove personas with the same ID (i.e. don't allow the recent persona
    // to appear twice on the list).  Afterwards, we'll add the recent persona
    // to the list in a way that makes it the most recent one.
    if (persona.id)
      personas = personas.filter(function(v) !v.id || v.id != persona.id);

    // Make the new persona the most recent one.
    personas.unshift(persona);

    // Note: at this point, there might be five personas on the list, four
    // that we parsed from preferences and the one we're now adding. But we
    // only serialize the first four back to preferences, so the oldest one
    // drops off the end of the list.

    // We store five in case we need to revert changes in the list alter on,
    // even though only four will be displayed.
    for (let i = 0; i < 5; i++) {
      if (i < personas.length)
        this._prefs.set("lastselected" + i, JSON.stringify(personas[i]));
      else
        this._prefs.reset("lastselected" + i);
    }
  },

  /**
   * Removes the most recent persona from the recent list, and leaves the
   * following four personas as the most recent.
   */
  _revertRecent: function() {
    // Create a new list of recent personas, removing the first one, but
    // including the 5th (hidden) one.
    let personas = this.getRecentPersonas(5);
    personas.shift();

    // Serialize the list of recent personas.
    for (let i = 0; i < 5; i++) {
      if (i < personas.length)
        this._prefs.set("lastselected" + i, JSON.stringify(personas[i]));
      else
        this._prefs.reset("lastselected" + i);
    }
  },

  onUseColorChanged: function() {
    // Notify observers that the persona has changed so the change in whether
    // or not to use the text or accent color will get applied.  The persona
    // hasn't really changed, but doing this has the desired effect without any
    // known unwanted side effects.
    Observers.notify("personas:persona:changed");
  },

  previewingPersona: null,

  /**
   * Display the given persona temporarily.  Useful for showing users who are
   * browsing the directory of personas what a given persona will look like
   * when selected, f.e. on mouseover.  Consumers who call this method should
   * call resetPersona when the preview ends, f.e. on mouseout.
   */
  previewPersona: function(persona) {
    if (LightweightThemeManager)
      LightweightThemeManager.previewTheme(persona);
    else {
      this.previewingPersona = persona;
      Observers.notify("personas:persona:changed");
    }
  },

  /**
   * Stop previewing a persona.
   */
  resetPersona: function() {
    if (LightweightThemeManager)
      LightweightThemeManager.resetPreview();
    else {
      this.previewingPersona = null;
      Observers.notify("personas:persona:changed");
    }
  },

  /**
   * Gets the persona specified by the initial_persona cookie.
   */
  _getPersonaFromCookie: function() {
    let authorizedHosts = this._prefs.get("authorizedHosts").split(/[, ]+/);
    let cookieManager =
      Cc["@mozilla.org/cookiemanager;1"].getService(Ci.nsICookieManager);
    let cookieEnu = cookieManager.enumerator;
    let selectedCookie = null;

    while (cookieEnu.hasMoreElements()) {
      let cookie = cookieEnu.getNext().QueryInterface(Ci.nsICookie);

      // XXX: Remove the initial dot from the host name, if any, before it is
      // compared against the authorized hosts. This fixes the bug reported in
      // bug 492392 that getpersonas.com and www.getpersonas.com cookies
      // imported from IE have the cookie host .getpersonas.com, so they didn't
      // match any of the authorized hosts.
      let cookieHost = cookie.host.replace(/^\./, "");

      if (cookie.name == COOKIE_INITIAL_PERSONA &&
          authorizedHosts.some(function(v) v == cookieHost)) {

        // There could be more than one "initial_persona" cookie. The cookie
        // with latest expiration time is selected.
        if (null == selectedCookie ||
            cookie.expires > selectedCookie.expires) {
          selectedCookie = cookie;
        }

        cookieManager.remove(cookie.host, cookie.name, cookie.path, false);
      }
    }

    if (selectedCookie)
      return JSON.parse(decodeURIComponent(selectedCookie.value));

    return null;
  },

  /**
   * Whether or not a user is signed in, determined by the presence of a cookie
   * named "PERSONA_USER".
   * @return True if signed in, false otherwise.
   */
  get isUserSignedIn() {
    let cookieManager =
      Cc["@mozilla.org/cookiemanager;1"].getService(Ci.nsICookieManager2);

    let userCookie = {
      QueryInterface: XPCOMUtils.generateQI([Ci.nsICookie2, Ci.nsICookie]),
      host: this._prefs.get("host"),
      path: "/",
      name: COOKIE_USER
    };

    return cookieManager.cookieExists(userCookie);
  },

  //**************************************************************************//
  // Lightweight Themes - Personas synchronization

  /**
   * Updates the add-on to reflect the changes from the Tools - Add-ons - Themes
   * dialog. If a lightweight theme is set, it is also set as the add-on's current
   * persona. If a regular theme is set, the current persona is set to "default".
   */
  onLightweightThemeChanged: function() {
    let currentTheme = LightweightThemeManager.currentTheme;
    if (currentTheme && currentTheme.id != this.currentPersona.id)
      this.changeToPersona(currentTheme);
    else if (!currentTheme && this.selected != "default")
      this.changeToDefaultPersona();
  },

  //**************************************************************************//
  // Random Rotation

  _rotationTimer : null,

  /**
   * Checks the current value of the "selected" preference and sets the
   * rotation timer accordingly.
   */
  onSelectedModeChanged: function() {
    this._rotationTimer.cancel();

    let selectedMode = this.selected;

    if (selectedMode == "random" || selectedMode == "randomFavorite") {
      let interval = this._prefs.get("rotationInterval") * 1000;
      let that = this;

      this._rotationTimer.initWithCallback(
        { notify: function(aTimer) { that._rotatePersona(); } },
        interval, Ci.nsITimer.TYPE_REPEATING_SLACK);

      this._rotatePersona();
    }
  },

  /**
   * Changes the current persona to a random persona of the same category (while
   * in "random" mode) or a random persona from he favorite list (while in
   * "randomFavorite" mode).
   */
  _rotatePersona : function() {
    switch (this.selected) {
      case "random":
        this.changeToRandomPersona(this.category);
        break;
      case "randomFavorite":
        this.changeToRandomFavoritePersona();
        break;
    }
  },

  //**************************************************************************//
  // Persona Images Caching

  /**
   * Caches the header and footer images of the given persona inside the
   * directory [profile]/personas/cache/[persona.id]. It removes all other
   * existing persona directories before doing so.
   * @param aPersona The persona for which to cache the images.
   */
  _cachePersonaImages : function(aPersona) {
    let cacheDirectory =
      FileUtils.getDirectory(FileUtils.getPersonasDirectory(), "cache");

    // Remove all other subdirectories in the cache directory
    // XXX: In the future, if we want to keep more than one persona cached
    // this step would be removed.
    let subdirs = FileUtils.getDirectoryEntries(cacheDirectory);
    for (let i = 0; i < subdirs.length; i++) {
      if (subdirs[i].isDirectory())
        subdirs[i].remove(true);
    }

    // Create directory for the given persona
    let personaDir = FileUtils.getDirectory(cacheDirectory, aPersona.id);

    // Save header if specified.
    let header = aPersona.headerURL || aPersona.header;
    if (header) {
      let headerURI = URI.get(header, null, URI.get(this.dataURL)).
                      QueryInterface(Ci.nsIURL);
      let headerCallback = function(aEvent) {
        let request = aEvent.target;
        // Save only if the folder still exists (Could have been deleted already)
        if (request.status == 200 && personaDir.exists()) {
          FileUtils.writeBinaryFile(
            personaDir.clone(),
            "header" + "." + headerURI.fileExtension,
            request.responseText);
        }
      };
      this._makeRequest(headerURI.spec, headerCallback, null, true);
    }

    // Save footer if specified.
    let footer = aPersona.footerURL || aPersona.footer;
    if (footer) {
      let footerURI = URI.get(footer, null, URI.get(this.dataURL)).
                      QueryInterface(Ci.nsIURL);
      let footerCallback = function(aEvent) {
        let request = aEvent.target;
        // Save only if the folder still exists (Could have been deleted already)
        if (request.status == 200 && personaDir.exists()) {
          FileUtils.writeBinaryFile(
            personaDir.clone(),
            "footer" + "." + footerURI.fileExtension,
            request.responseText);
        }
      };
      this._makeRequest(footerURI.spec, footerCallback, null, true);
    }
  },

  /**
   * Obtains the cached images of the given persona. This are stored in the
   * _cachePersonaImages method under the directory
   * [profile]/personas/cache/[persona.id].
   * @param aPersona The persona for which to look the cached images.
   * @return An object with "header" and "footer" properties, each containing
   * the file URL of the image. Null otherwise.
   */
  getCachedPersonaImages : function(aPersona) {
    let cacheDirectory =
      FileUtils.getDirectory(FileUtils.getPersonasDirectory(), "cache");

    let personaDir = FileUtils.getDirectory(cacheDirectory, aPersona.id, true);
    if (personaDir.exists()) {

      let headerFile = personaDir.clone();
      let footerFile = personaDir.clone();

      let headerFileExtension =
        URI.get(aPersona.headerURL || aPersona.header, null, URI.get(this.dataURL)).
        QueryInterface(Ci.nsIURL).fileExtension;

      let footerFileExtension =
        URI.get(aPersona.footerURL || aPersona.footer, null, URI.get(this.dataURL)).
        QueryInterface(Ci.nsIURL).fileExtension;

      headerFile.append("header" + "." + headerFileExtension);
      footerFile.append("footer" + "." + footerFileExtension);

      if (headerFile.exists() && footerFile.exists()) {
        let ios =
          Cc["@mozilla.org/network/io-service;1"].getService(Ci.nsIIOService);

        let headerURI = ios.newFileURI(headerFile);
        let footerURI = ios.newFileURI(footerFile);

        return {
          header : headerURI.spec,
          footer : footerURI.spec
        };
      }
    }
    return null;
  },

  /**
   * Monitors changes in cookies. If the modified cookie is the Personas session
   * cookie, then the favorites are refreshed (if the user is signed in).
   * @param aCookie The cookie that has been added, changed or removed.
   */
  onCookieChanged : function(aCookie) {
    aCookie.QueryInterface(Ci.nsICookie);

    if (aCookie.name == COOKIE_USER &&
        aCookie.host == this._prefs.get("host")) {
      this.refreshFavorites();
    }
  },

  onQuitApplication: function() {
    Observers.remove("quit-application", this.onQuitApplication, this);
    this._destroy();
  }
};

let DateUtils = {
  /**
   * Returns the number as a string with a 0 prepended to it if it contains
   * only one digit, for formats like ISO 8601 that require two digit month,
   * day, hour, minute, and second values (f.e. so midnight on January 1, 2009
   * becomes 2009:01:01T00:00:00Z instead of 2009:1:1T0:0:0Z, which would be
   * invalid).
   */
  _pad: function(number) {
    return (number >= 0 && number <= 9) ? "0" + number : "" + number;
  },

  /**
   * Format a date per ISO 8601, in particular the subset described in
   * http://www.w3.org/TR/NOTE-datetime, which is recommended for date
   * interchange on the internet.
   *
   * Example: 1994-11-06T08:49:37Z
   *
   * @param   date  {Date}    the date to format
   * @returns       {String}  the date formatted per ISO 8601
   */
  toISO8601: function(date) {
    let year = date.getUTCFullYear();
    let month = this._pad(date.getUTCMonth() + 1);
    let day = this._pad(date.getUTCDate());
    let hours = this._pad(date.getUTCHours());
    let minutes = this._pad(date.getUTCMinutes());
    let seconds = this._pad(date.getUTCSeconds());
    return year + "-" + month + "-" + day + "T" +
           hours + ":" + minutes + ":" + seconds + "Z";
  },

  /**
   * Format a date per RFC 1123, which is the standard for HTTP headers.
   *
   * Example: Sun, 06 Nov 1994 08:49:37 GMT
   *
   * I'd love to use Datejs here, but its Date::toString formatting method
   * doesn't convert dates to their UTC equivalents before formatting them,
   * resulting in incorrect output (since RFC 1123 requires dates to be
   * in UTC), so instead I roll my own.
   *
   * @param   date  {Date}    the date to format
   * @returns       {String}  the date formatted per RFC 1123
   */
  toRFC1123: function(date) {
    let dayOfWeek = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"][date.getUTCDay()];
    let day = this._pad(date.getUTCDate());
    let month = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"][date.getUTCMonth()];
    let year = date.getUTCFullYear();
    let hours = this._pad(date.getUTCHours());
    let minutes = this._pad(date.getUTCMinutes());
    let seconds = this._pad(date.getUTCSeconds());
    return dayOfWeek + ", " + day + " " + month + " " + year + " " +
           hours + ":" + minutes + ":" + seconds + " GMT";
  }
};

let FileUtils = {
  /**
   * Gets the [profile]/personas directory.
   * @return The reference to the personas directory (nsIFile).
   */
  getPersonasDirectory : function() {
    let directoryService =
      Cc["@mozilla.org/file/directory_service;1"].getService(Ci.nsIProperties);
    let dir = directoryService.get("ProfD", Ci.nsIFile);

    return this.getDirectory(dir, "personas");
  },

  /**
   * Gets a reference to a directory (nsIFile) specified by the given name,
   * located inside the given parent directory.
   * @param aParentDirectory The parent directory of the directory to obtain.
   * @param aDirectoryName The name of the directory to obtain.
   * @param aDontCreate (Optional) Whether or not to create the directory if it
   * does not exist.
   * @return The reference to the directory (nsIFile).
   */
  getDirectory : function(aParentDirectory, aDirectoryName, aDontCreate) {
    let dir = aParentDirectory.clone();
    try {
      dir.append(aDirectoryName);
      if (!dir.exists() || !dir.isDirectory()) {
        if (!aDontCreate) {
          // read and write permissions to owner and group, read-only for others.
          dir.create(Ci.nsIFile.DIRECTORY_TYPE, 0774);
        }
      }
    }
    catch (ex) {
      Cu.reportError("Could not create '" + aDirectoryName + "' directory");
      dir = null;
    }
    return dir;
  },

  /**
   * Gets an array of the entries (nsIFile) found in the given directory.
   * @param aDirectory The directory from which to obtain the entries.
   * @return The array of entries.
   */
  getDirectoryEntries : function(aDirectory) {
    let entries = [];
    try {
      let enu = aDirectory.directoryEntries;
      while (enu.hasMoreElements()) {
        let entry = enu.getNext().QueryInterface(Ci.nsIFile);
        entries.push(entry);
      }
    }
    catch (ex) {
      Cu.reportError("Could not read entries of directory");
    }
    return entries;
  },

  /**
   * Reads the contents of a text file located at the given directory.
   * @param aDirectory The directory in which the file is read from (nsIFile)
   * @param aFileName The name of the file to be read.
   * @return The contents of the file (string), if any.
   */
  readFile : function(aDirectory, aFileName) {
    let data = "";

    try {
      let file = aDirectory.clone();
      file.append(aFileName);

      if (file.exists()) {
        let fstream =
          Cc["@mozilla.org/network/file-input-stream;1"].
            createInstance(Ci.nsIFileInputStream);
        fstream.init(file, -1, 0, 0);

        let cstream =
          Cc["@mozilla.org/intl/converter-input-stream;1"].
            createInstance(Ci.nsIConverterInputStream);
        cstream.init(fstream, "UTF-8", 0, 0);

        let (str = {}) {
          // read the whole file
          while (cstream.readString(-1, str))
            data += str.value;
        }
        cstream.close(); // this also closes fstream
      }
    }
    catch (ex) {
      Cu.reportError("Could not read file " + aFileName);
    }

    return data;
  },

  /**
   * Writes a text file in the given directory. If the file already exists it is
   * overwritten.
   * @param aDirectory The directory in which the file will be written (nsIFile).
   * @param aFileName The name of the file to be written.
   * @param aData The contents of the file.
   */
  writeFile : function(aDirectory, aFileName, aData) {
    try {
      let file = aDirectory.clone();
      file.append(aFileName);

      let foStream =
        Cc["@mozilla.org/network/file-output-stream;1"].
          createInstance(Ci.nsIFileOutputStream);
      // flags are write, create, truncate
      foStream.init(file, 0x02 | 0x08 | 0x20, 0666, 0);

      let converter =
        Cc["@mozilla.org/intl/converter-output-stream;1"].
          createInstance(Ci.nsIConverterOutputStream);
      converter.init(foStream, "UTF-8", 0, 0);
      converter.writeString(aData);
      converter.close(); // this also closes foStream
    }
    catch (ex) {
      Cu.reportError("Could not write file " + aFileName);
    }
  },

  /**
   * Writes a binary file in the given directory. If the file already exists it
   * is overwritten.
   * @param aDirectory The directory in which the file will be written (nsIFile).
   * @param aFileName The name of the file to be written.
   * @param aData The binary contents of the file.
   */
  writeBinaryFile : function(aDirectory, aFileName, aData) {
    try {
      let file = aDirectory.clone();
      file.append(aFileName);

      let stream =
        Cc["@mozilla.org/network/safe-file-output-stream;1"].
          createInstance(Ci.nsIFileOutputStream);
      // Flags are: write, create, truncate
      stream.init(file, 0x04 | 0x08 | 0x20, 0600, 0);

      stream.write(aData, aData.length);
      if (stream instanceof Ci.nsISafeOutputStream)
        stream.finish();
      else
        stream.close();
    }
    catch (ex) {
      Cu.reportError("Could not write binary file " + aFileName);
    }
  }
};

PersonaService._init();
