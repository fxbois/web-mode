function x() {
  const good = (
	<div
	  attr={3}
	  otherattr={5}>
	  wat
	</div>
  );
  const bad = (
	<CustomComponent
	  attr={5}
	  otherattr={3}
	/>
  );
  const badish = (
	<CustomComponent with
			         multiple attributes
			         per line somehow
			         works
			         until
			         you
			         include
			         an={sign}
		             and then it's
			         back to unindented until
			         you have another multi-attr line
	/>
  );
}
