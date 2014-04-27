
#|
;; BASIC TEST
(apply-command (system-get 'main-command-line-interpreter) 
               '(GE025.exe ":command-file" "d:\\vrp_500_0003.command"))


;;; #CASE 1 - Background search of a task from a file location
(register-task-path 'example-task-vrp-001 "d:\\tasks\\Examples_VRP_001.task")
(execute-registered-task 'example-task-vrp-001)
(report-results :task 'example-task-vrp-001 :format :basic :path "Examples_VRP_001.result" :mode :append)


;;; #CASE 2 - Background search of a task group from a file location
(register-task-group
 'examples-group-vrp
 :path "\tasks\Examples_VRP_001.task" :task 'example-task-vrp-001
 :path "\tasks\Examples_VRP_002.task" :task 'example-task-vrp-002
 :path "\tasks\Examples_VRP_003.task" :task 'example-task-vrp-003
 :path "\tasks\Examples_VRP_004.task" :task 'example-task-vrp-004
 :path "\tasks\Examples_VRP_005.task" :task 'example-task-vrp-005)
(execute-registered-task-group 'example-group-vrp)
(report-group-results :group 'examples-group-vrp :format :basic :path "Examples_VRP_001.result" :mode :append)


;;; #CASE 3 - Network search and GUI for monitoring
(set-distributed-environment-configuration :path "\environments\local-network.distributed-environment-data")
(load-default-panes :path "\environments\default-panes-001.environment")
(register-task-group
	:group 'examples-group-vrp
	:path "\tasks\Examples_VRP_001.task" :task 'example-task-vrp-001
	:path "\tasks\Examples_VRP_002.task" :task 'example-task-vrp-002
	:path "\tasks\Examples_VRP_003.task" :task 'example-task-vrp-003
	:path "\tasks\Examples_VRP_004.task" :task 'example-task-vrp-004
	:path "\tasks\Examples_VRP_005.task" :task 'example-task-vrp-005)
(execute-registered-task-group 'example-group-vrp)
(report-group-results :group 'examples-group-vrp :format :basic :path "Examples_VRP_002 - Local network.result" 
                      :mode :append)
|#
