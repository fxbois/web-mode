function s(action) {
  switch(action.type) {
	case SUBSCRIBE:
	  // TODO some comment here
	  // another line here
	  // line ending with a period.
	  resource = action.resourceDescription.resource;
	  return 0;
	case UNSUBSCRIBE:
	  return 1;
	default:
	  return 2;
  }
}
