/*
 * Returns a constructor.
 * 
 * The parameter is an object whose properties are
 * copied into the constructor's prototype.
 * */
var Builder = function(body) {
  var constructor = function() {    
      this.init.apply(this, arguments);          
  };  

  for (var property in body) { 
     constructor.prototype[property] = body[property];
  }
        
  if (!constructor.prototype.init) constructor.prototype.init = function(){};      
  
  return constructor;    
}

var Wall = Builder({
  init: function(key, title, count) {
    this.key = key;
    this.title = title;
    this.count = count;
  },

  getKey: function() {
    return this.key;
  },

  getTitle: function() {
    return this.title;
  },

  setTitle: function(title) {
    this.title = title;
  },

  getCount: function() {
    return this.count;
  },

  setCount: function(count) {
    this.count = count;
  }
});

/*
 * Local storage
 * 
 * Either by "true" local storage, session storage, or cookies.
 * */
var WallStorage = Builder({
  init: function() {
    this.storage = $.initNamespaceStorage('thewall').localStorage;
    if (!this.storage) {
      this.storage = $.initNamespaceStorage('thewall').sessionStorage;
    }
    if (!this.storage) {
      this.storage = $.initNamespaceStorage('thewall').cookieStorage;
    }
  },

  getAll: function() {
    var walls = this.storage.get('walls');

    if (!walls) {
      this.setWalls([]);
    }

    var ret = [];

    for (var i in walls) {
      // Turn plain JSON objects into Wall objects
      ret.push(new Wall(walls[i].key, walls[i].title, walls[i].count));
    }

    return ret;
  },

  setAll: function(walls) {
    this.storage.set('walls', walls);
  },

  add: function(wall) {
    var walls = this.getAll();

    for (var i in walls) {
      if (walls[i].getKey() == wall.getKey()) {
        return;
      }
    }

    walls.push(wall);

    this.storage.set('walls', walls); // TODO: do we need this?
  },

  remove: function(key) {
    var walls = this.getAll();
    var newWalls = [];
    
    for (var i in walls) {
      if (walls[i].getKey() != key) {
        newWalls.push(walls[i]);
      }
    }

    this.setAll(newWalls);
  }

});

/*
 * Menu handling
 * */
var WallMenu = Builder({
  _addListeners: [],
  _removeListeners: [],
  _wallLinkListeners: [],
  _walls: {},

  init: function(storage, imp, exp) {
    this.storage = storage;
    this.import = imp;
    this.export = exp;

    var self = this;

    this.import.onImport(function(contents) {
      for (var i in contents) {
        self.add(contents[i]);
      }
    });

    $(function() {
      var navmenu = $('nav#menu');

      navmenu.mmenu({
        classes: "mm-light"
      });

      $("#nowalls").on( "click", function(e) {
        e.stopPropagation();
        e.preventDefault();
        self.add(new Wall(self.randomKey(), 'untitled', 0));
      });

      $("#addnew").on( "click", function(e) {
        e.stopPropagation();
        e.preventDefault();
        self.add(new Wall(self.randomKey(), 'untitled', 0));
      });

      $("#addkey").on( "click", function(e) {
        e.stopPropagation();
        e.preventDefault();
        var key = prompt('Enter a wall key');
        if (key !== null) {
            self.add(new Wall(key, 'untitled', 0));
        }
      });

      $("#export").on( "click", function(e) {
        e.stopPropagation();
        e.preventDefault();
        self.export.export(JSON.stringify(self.storage.getAll()));
      });

      $("#import").on( "click", function(e) {
        e.stopPropagation();
        e.preventDefault();
      });

      $("#delall").on( "click", function(e) {
        e.stopPropagation();
        e.preventDefault();
        if (confirm('Sure?')) {
          for (var key in self._walls) {
            self.remove(key);
          }
        }
      });

      self.loadMenu();

    });

  },

  randomKey: function()
  {
    var text = "";
    var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

    for( var i = 0; i < 32; i++ ) {
      text += possible.charAt(Math.floor(Math.random() * possible.length));
    }

    return text;
  },

  loadMenu: function() {
    var walls = this.storage.getAll();
    for (var i in walls) {
      this.add(walls[i]);
    }
  },

  /*
   * Add a new wall link to the menu
   * */
  add: function(wall) {
    this.storage.add(wall);
    this._walls[wall.getKey()] = wall;

    var html = '<li class="wall" id="' + wall.getKey() + '"><a href="#' + wall.getKey() + '"><img src="img/wireframe_mono/blacks/16x16/spechbubble_2.png"/> ' + wall.getCount() + ' ';
    if (wall.getCount() > 0) {
      html += '<strong>' + wall.getTitle() + ' (Zg9mDA1IXmPejy3KmANf7oq3Cj0ouqEa)</strong>';
    } else {
      html += wall.getTitle() + ' (' + wall.getKey() + ') ';
    }
    html += '<img src="img/wireframe_mono/blacks/16x16/delete.png" style="position:absolute;right:10px" /></a></li>\n'

    var nowalls = $('#nowalls');

    nowalls.hide();
    nowalls.after(html);

    var self = this;

    $('#' + wall.getKey()).click(function(e) {
        e.preventDefault();
        e.stopPropagation();

        var thisS = $(this);
        var clickDistanceFromRight = thisS.width() - e.pageX + thisS.position().left;

        if (clickDistanceFromRight < 35) {
            if (confirm('Sure?')) {
                self.remove(wall.getKey());
            }
        } else {
            self._onWallLink(wall);
        }
    });

    this._onAdd(wall);
  },

  _onAdd: function(key) {
      for (var i in this._removeListeners) {
        this._addListeners[i](key);
      }
  },

  onAdd: function (listener) {
    this._addListeners.push(listener);
  },

  remove: function(key) {
      $('#' + key).remove();
      this.storage.remove(key);
      this._onRemove(this._walls[key]);
      delete this._walls[key];
      if (Object.keys(this._walls).length == 0) {
        $('#nowalls').show();
      }
  },

  _onRemove: function (wall) {
      for (var i in this._removeListeners) {
        this._removeListeners[i](wall);
      }
  },

  onRemove: function (listener) {
    this._removeListeners.push(listener);
  },

  _onWallLink: function (wall) {
    for (var i in this._wallLinkListeners) {
      this._wallLinkListeners[i](wall);
    }
  },

  onWallLink: function (listener) {
    this._wallLinkListeners.push(listener);
  }
});

var WallImport = Builder({
  _importListeners: [],

  init: function() {
    if (FileReader)
    {
      var self = this;
      $(function() {
        $('#fileinput').change(self._getFileChangeHandler());
      });
    }
  },

  /* 
   * Reads a file with data to import
   * */
  _getFileChangeHandler: function() {
    var self = this;

    return function(evt) {
      var f = evt.target.files[0];

      if (!f) {
        alert('Could not read import file');
        return;
      }

      var r = new FileReader();

      r.onload = function(e) {
        try {
          var contents = JSON.parse(e.target.result);
          console.log(self);
          var walls = [];
          for (var i in contents) {
            walls.push(new Wall(contents[i].key, contents[i].title, contents[i].count));
          }
          self._onImport(walls);
        } catch (e) {
          alert('The file contents could not be interpreted' );
          return;
        }
      };

      r.readAsText(f);
    }
  },

  _onImport: function(contents) {
    for( var i in this._importListeners) {
      this._importListeners[i](contents);
    }
  },

  onImport: function(listener) {
    this._importListeners.push(listener);
  }
});

var WallExport = Builder({
  init: function(storage) {
  },

  export: function(object) {
    var blob = new Blob([JSON.stringify(object)], {type: "application/json"});
    saveAs(blob, "walls.json");
  }
});

var WallBackend = Builder({
  init: function(self) {
  }
});

var WallApp = Builder({
  init: function(self) {
    this.storage = new WallStorage();

    this.import = new WallImport();
    this.export = new WallExport();

    this.menu = new WallMenu(this.storage, this.import, this.export);

    this.backend = new WallBackend();

    this.menu.onWallLink(this.wallLinkClicked);
    this.menu.onAdd(this.wallAdded);
    this.menu.onRemove(this.wallRemoved);
  },

  wallLinkClicked: function(wall) {
    alert(wall.title);
  },

  wallAdded: function(wall) {
  },

  wallRemoved: function(wall) {
  },
});

var wallApp = new WallApp();

/* vim: set tabstop=2:softtabstop=2:shiftwidth=2:expandtab */
