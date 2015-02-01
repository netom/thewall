TheWall.Post = DS.Model.extend({
  time: DS.attr('date'),
  nick: DS.attr('string'),
  body: DS.attr('string'),
  seen: DS.attr('boolean'),
  wall: DS.belongsTo('wall')
});

TheWall.Post.FIXTURES = [
 {
   id: 1,
   nick: 'netom',
   body: 'Helló',
   seen: true,
   wall: 1
 },
 {
   id: 2,
   nick: 'Tibi',
   body: 'Helló',
   seen: false,
   wall: 1
 },
 {
   id: 3,
   nick: 'Tibi',
   body: 'Mi a helyzet?',
   seen: false,
   wall: 1
 },
 {
   id: 4,
   nick: 'netom',
   body: 'Helló',
   seen: true,
   wall: 2
 },
 {
   id: 5,
   nick: 'Imre',
   body: 'Helló',
   seen: true,
   wall: 2
 },
 {
   id: 6,
   nick: 'Hogy ityeg?',
   body: 'Helló',
   seen: true,
   wall: 3
 },
 {
   id: 7,
   nick: 'tomi',
   body: 'Helló',
   seen: true,
   wall: 3
 },
 {
   id: 8,
   nick: 'Dante',
   body: 'Sziasztok!',
   seen: false,
   wall: 3
 }
];

