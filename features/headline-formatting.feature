Feature: Headline formatting
  Scenario: Should be normalized (indent level = 1)
    When I create an org file with content:
      """
      *** Tags :article:
      Features and scenarios can be tagged using syntax @tag
      **** More indentations
      """
    And I create headline from element at point
    Then headline title should be "Tags"
    And headline should be an article
    And headline contents should be:
      """
      * Tags :article:
      Features and scenarios can be tagged using syntax @tag
      ** More indentations
      """

  Scenario: No links in title allowed
    When I create an org file with content:
      """
      * Transmission bookmark: [[http://127.0.0.1:9091/transmission/web/#upload][Transmission Web UI]] :bookmark:
      - Uses fewer resources than other clients
      """
    And I create headline from element at point
    Then headline title should be "Transmission bookmark: Transmission Web UI"
    And headline should be a bookmark
    And headline contents should be:
      """
      * Transmission bookmark: [[http://127.0.0.1:9091/transmission/web/#upload][Transmission Web UI]] :bookmark:
      - Uses fewer resources than other clients
      """
