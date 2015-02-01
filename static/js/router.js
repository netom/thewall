TheWall.Router.map(function() {
  this.resource('walls', { path: '/' });
});

TheWall.WallsRoute = Ember.Route.extend({
  model: function() {
    return this.store.find('wall');
  }
});
