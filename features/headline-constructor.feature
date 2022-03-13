Feature: Headline
  Scenario: Should be creatable from org headline at point
    When I create an org file with content:
      """
      * Google SRE Book :book:article:
      Managing Incidents
      """
    And I create headline from element at point
    Then headline title should be "Google SRE Book"
    And headline should be a book
    And headline should be an article
    And headline contents should be:
      """
      * Google SRE Book :book:article:
      Managing Incidents
      """

  Scenario: Should be creatable from the inside
    When I create an org file with content:
      """
      * TODO Vocal lesson :Task:
      - Mozart
      - Bach
      """
    And I goto the end of the buffer
    And I create headline from element at point
    Then headline title should be "Vocal lesson"
    And headline should be a task
    And headline contents should be:
      """
      * TODO Vocal lesson :Task:
      - Mozart
      - Bach
      """
