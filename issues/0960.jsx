render() {
  return (
	<div>
	  <Navbar color="faded" light expand="md">
		<NavbarBrand href="/">datalibrary</NavbarBrand>
		<NavbarToggler onClick={this.toggle} />
		<Collapse isOpen={this.state.isOpen} navbar>
		  <Nav className="ml-auto" navbar>
            <NavItem>
              <NavLink href="/files/">Files</NavLink>
            </NavItem>
			<UncontrolledDropdown nav inNavbar>
			  <DropdownToggle nav caret>
				Options
			  </DropdownToggle>
			  <DropdownMenu >
				<DropdownItem>
				  Option 1
				</DropdownItem>
				<DropdownItem>
				  Option 2
				</DropdownItem>
				<DropdownItem divider />
				<DropdownItem>
				  Reset
				</DropdownItem>
			  </DropdownMenu>
			</UncontrolledDropdown>
			<NavItem>
			  <SearchBox/>
			</NavItem>
		  </Nav>
		</Collapse>
	  </Navbar>
	</div>
  );
}
