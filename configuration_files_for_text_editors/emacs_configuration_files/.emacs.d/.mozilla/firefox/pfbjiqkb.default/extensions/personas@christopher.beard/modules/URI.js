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
 * The Original Code is URI.
 *
 * The Initial Developer of the Original Code is Mozilla.
 * Portions created by the Initial Developer are Copyright (C) 2008
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
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

const EXPORTED_SYMBOLS = ["URI"];

const Cc = Components.classes;
const Ci = Components.interfaces;
const Cr = Components.results;
const Cu = Components.utils;

/**
 * A URI.  For now, this returns an nsIURI rather than an instance of this
 * class, but in the future it might return an instance of this class and have
 * a more JS-friendly API for accessing and manipulating the URI.
 */
function URI(spec, charset, baseURI) {
  return URI.ioSvc.newURI(spec, charset, baseURI);
}

/**
 * Get a URI.  Similar to the constructor, but returns null instead of throwing
 * an exception if the URI object could not be constructed.
 */
URI.get = function(spec, charset, baseURI) {
  try {
    return new URI(spec, charset, baseURI);
  }
  catch(ex) {
    return null;
  }
};

URI.__defineGetter__("ioSvc",
  function() {
    delete this.ioSvc;
    return this.ioSvc = Cc["@mozilla.org/network/io-service;1"].
                        getService(Ci.nsIIOService);
  }
);
