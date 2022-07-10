Feature: Store
  Scenario: Import headlines from directory recursively
    Given store "TaskStore"
    Given file "household.org" in directory "tasks/home"
      """
      * TODO Buy milk :Task:
      * TODO Buy eggs :Task:
      """
    Given file "tasks.org" in directory "tasks/office"
      """
      * TODO Read mail :Task:
      * TODO Procrastinate :Task:
      """
    Given file "tasks.org.~undo-tree~" in directory "tasks/office"
      """
      * Messy stuff of undo-tree, we should ignore it
      """
    When I import headlines from directory "tasks" to store "TaskStore"
    Then store "TaskStore" should contain 4 tasks
