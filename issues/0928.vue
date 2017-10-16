<script>
 export default {
   beforeRouteEnter(from, to, next) {
     EventBus.$emit(events.RENDER_SIDEBAR, {
       view: 'items',
       items: store.getters.getItems.map(m => ( // press Enter here => tab inserted
         {
           m.title,
         }
       )),
     });
   }
 };
</script>
