Feature: Registry
  Scenario: Add headline to registry
    Given registry "dogs"
    Given headline "eric"
      """
      * Eric :dog:
      """

    # Then "eric" should not be registered in "dogs"
    # When I add "eric" to "dogs" as "eric-the-dog"
    # Then registry "dogs" should contain 1 headline
    # And "eric-the-dog" should be registered in "dogs"
    # And "eric" should not be registered in "dogs"

    # When I get headline by title "Eric" from registry "dogs"
    # Then this headline contents should be:
    #   """
    #   * Eric :dog:
    #   """

  # Scenario: Serializable
