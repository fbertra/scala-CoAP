VERY LONG TERM GOAL: run a scala-native app using the CoAP protocol on a IoT device.  The IoT device should run an micro kernel like LK Littlekernel, Solo5 or other embedded OS.

Links
-----

- CoAP is an internet protocol targeting the Internet of Things. See https://tools.ietf.org/html/rfc7252 and http://coap.technology/

- scala-native compiles Scala source code to native via LLVM. http://scala-native.org, https://github.com/scala-native/scala-native and http://llvm.org/

- LK is an micro-OS used as bootloader in some Android devices and is part of Google Fuchsia iniciative. https://github.com/littlekernel/lk and https://github.com/fuchsia-mirror

- Solo5 is another micro-OS used in MirageOS, an Unikernel based on the Ocaml language.  https://github.com/Solo5/solo5 and https://mirage.io/


Notes
--------

- scala-native targets posix/64-bits architecture and uses the Boehm Garbage Collector, so it is currently restricted to "big" devices like Raspberry PI based on Linux.  Support for 32-bits and another GC are planned (https://github.com/scala-native/scala-native/issues/143 and https://github.com/scala-native/scala-native/issues/128).  Support for non-posix systems isn't planned

- TCP/IP. scala-native doesn't support the java net library yet.   The java net library follows the Posix socket model (a connection abstraction between endpoints) and IoT prefer message passing model like in CoAP.  So, the socket model doesn't seem to be a good abstraction.  Finally, LK provide a mini-ip stack and it isn't Posix/socket compatible.

- Threading, scala-native is currently mono-thread.  LK supports multi-thread so the CoAP stack could run in its own thread and use message passing with client apps.  This is a model common in scala land (as in akka framework)

- Link layer.  CoAP sits on top of UDP.  The goal is to support at least WiFI and 6LowPAN/804.15.4 as UDP support.

- Devices.  The goal is to support battery based 32-bits devices with at least 4MB RAM. 

Steps
-----

1- create an mini CoAP stack in scala and test it on the JVM.

2- cross compile it with scala-native.

3- run it on a "big IoT" device like Rasberry PI

4- run a scala-native hello world app above LK or Solo5 and run it on a smaller IoT device

5- port the JVM CoAP apps over LK or Solo5

Current state
-----------------

Step 1, implementing the CoAP protocol on top of java net and Linux TPC/IP.