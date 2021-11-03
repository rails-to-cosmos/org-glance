Feature: Headline API

  @headline
  Scenario: Create from element at point
    When I create a file called "basic-headline.org" with content:
"""
* Google SRE Book :book:
Managing Incidents
"""
    And I find file "basic-headline.org"
    Then headline title should be "Google SRE Book"
    And headline contents should be:
"""
* Google SRE Book :book:
Managing Incidents
"""

  @headline
  Scenario: Create from indented element at point
    When I create a file called "indented-headline.org" with content:
      """
      ** Tags :doc:
      Features and scenarios can be tagged using syntax @tag
      """
    And I find file "indented-headline.org"
    Then headline title should be "Tags"
    And headline contents should be:
      """
      * Tags :doc:
      Features and scenarios can be tagged using syntax @tag
      """

  @headline
  Scenario: Create from element with links in title
    When I create a file called "headline-with-links.org" with content:
      """
      * Transmission bookmark: [[http://127.0.0.1:9091/transmission/web/#upload][Transmission Web UI]] :bookmark:
      - Uses fewer resources than other clients
      """
    And I find file "headline-with-links.org"
    Then headline title should be "Transmission bookmark: Transmission Web UI"
    And headline contents should be:
      """
      * Transmission bookmark: [[http://127.0.0.1:9091/transmission/web/#upload][Transmission Web UI]] :bookmark:
      - Uses fewer resources than other clients
      """

  @headline
  Scenario: Create from the inside
    When I create a file called "headline-from-inside.org" with content:
      """
      * Vocal lesson :task:
      - Mozart
      - Bach
      """
    And I find file "headline-from-inside.org"
    And I goto the end of the buffer
    Then headline title should be "Vocal lesson"
    And headline contents should be:
      """
      * Vocal lesson :task:
      - Mozart
      - Bach
      """
