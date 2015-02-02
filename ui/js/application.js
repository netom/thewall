/*window.onerror = function (msg, url, line, col, error) {
  console.log(error);
}*/

window.TheWall = Ember.Application.create();

TheWall.ApplicationAdapter = DS.FixtureAdapter.extend();

/*TheWall.ApplicationAdapter = DS.LSAdapter.extend({
  namespace: 'thewall-emberjs'
});*/

/*!
 *
 *  Copyright (c) David Bushell | http://dbushell.com/
 *
 */
var c = 0;

Ember.View.reopen({
  didInsertElement : function(){
    this._super();

    if (c > 0) {
      return;
    }

    c++;

    Ember.run.scheduleOnce('afterRender', this, this.afterRenderEvent);
  },

  afterRenderEvent : function(){
    var mb = $('#menubutton');
    var c =  $('#content');
    var m =  $('#menu');

    mb.click(function() {
      m.toggleClass('menuopen');
      c.toggleClass('menuopen');
    });
  }
});
