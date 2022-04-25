Feature: Registry
  Scenario: Put and get
    Given registry "dogs"
    And headline "eric"
      """
      * Eric :dog:
      """

    Then headline "eric" should not have an ID in registry "dogs"

    When I put headline "eric" into registry "dogs"
    Then registry "dogs" should contain 1 headline
    And headline "eric" should have an ID in registry "dogs"

    # When I get headline by title "Eric" from registry "dogs"
    # Then this headline contents should be:
    #   """
    #   * Eric :dog:
    #   """

  # Scenario: Serializable
