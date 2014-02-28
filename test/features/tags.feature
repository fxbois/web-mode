Feature: Tags

  Background:
    Given I turn on web-mode

  Scenario: Automatically close tag
    Given I type "<script>"
    When I press "<"
    And I press "/"
    When I debug
    Then I should see "<script></script>"
    And the cursor should be between "<script>" and "</script>"
