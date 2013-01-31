# Quark Games Erlang Lightning Workshops

## Week I Specifying the chat server

### Sum up Erlang in a few sentences

A programming language featuring lightweight processes (make millions
on a machine) that share no memory (aka people/actors) which can
communicate with each other asynchronously (mailbox) or synchronously
(telephone). Communication can occur between people regardless of
where they live (on the same machine or different) with the same
syntax.

Erlang's scheduler is latency optimized (not throughput optimized).

### Chat Server

#### Requirements

+ Server in the cloud, client in the browser
+ Handle multiple (many) users
+ Handle multiple rooms
+ Notify users of joins and leaves
+ Handle chat (duh)
+ Be extensible enough to add functionality later if we need it
(developers all groan at the sound of this one)

How should we design this chat server?

#### Option A

+ One process to handle the entire event loop
+ Users from the browser connect to process, send/receive data
+ All state is contained in this monolithic program

#### Option B

+ Two processes, one to handle all the user connections (process X), one to handle
the chat room itself (process Y)
+ Users from the browser connect to process X and send and receive
messages from X.
+ X notifies Y of when users issue chats and process Y relays messages back to
X, indicating for each message what users should receive it, depending
on chat room state.
+ State is shared between X and Y so Y knows who's online and what
rooms they are in.

#### Option C

+ One process *per user* and one process *per chat room*.
+ Users from the browser talk to their designated process.
+ That process communicates with the corresponding chat room process
+ All state about a particular room is contained in that room's
process. All state about a particular user is contained in that user's process.

What option would you take?

---

Erlang programmers choose option C every time. This is not the correct
choice in other programming languages! For most languages, anything
but option A will take the unwitting programmer down the road to
madness and is hard to build safely and in a reasonable
amount of time (multithreading, STM, semaphores, mutexes, etc... =
hard).

## Specification

### Chat Room Process Requirements

#### init

+ Room name
+ Optional room topic

#### terminate

+ Delete room

#### change_topic

+ Change room topic

#### emit

+ Broadcast message to all users in room

#### receive

+ Receive a message from a user

#### join

+ Add a user to the roster

#### leave

+ Remove a user from the roster

### User Process Requirements

#### init

+ Called when a user logs on
+ Initializes with user name

#### terminate

+ Destroy process when user leaves

#### send

+ Send a message to a chat channel

#### receive

+ Receive a chat message from a channel and send it to client

#### join_room

+ Join a chat room

#### leave_room

+ Leave a chat room
