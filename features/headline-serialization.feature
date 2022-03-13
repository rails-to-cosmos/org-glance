Feature: Headline
  Scenario: Should be serializable
    When I create an org file with content:
      """
      *** Driving lesson :Education:
      - Key: Value
      """
    And I create headline from element at point
    And I save headline to file "driving.org"
    And I load headline from file "driving.org"
    Then headline title should be "Driving lesson"
    And headline should be an education
    And headline should be propertized
    And headline contents should be:
      """
      * Driving lesson :Education:
      - Key: Value
      """
