const { nanoid } = require('nanoid');
const OT = require('@opentok/client');


export function initialClientPorts(elmApp) {
    var apiKey;
    var session;
    var sessionId;
    var token;

    elmApp.ports.initializeClient.subscribe(function (config) {
        apiKey = config.apiKey;
        sessionId = config.sessionId;
        token = config.token;

        session = OT.initSession(apiKey, sessionId);

        // Subscribe to a newly created stream
        session.on('streamCreated', function streamCreated(event) {
            var subscriberOptions = {
                insertMode: 'append',
                width: '100%',
                height: '100%'
            };
            session.subscribe(event.stream, 'subscriber', subscriberOptions, function callback(error) {
                if (error) {
                    console.error('There was an error publishing: ', error.name, error.message);
                }
            });

            signalNewStreamConnection(event);
        });

        session.on('sessionDisconnected', function sessionDisconnected(event) {
            console.error('You were disconnected from the session.', event.reason);
        });

        // Initialize the publisher
        var publisherOptions = {
            insertMode: 'append',
            width: '100%',
            height: '100%',
            name: config.chatHandle
        };
        var publisher = OT.initPublisher('publisher', publisherOptions, function initCallback(initErr) {
            if (initErr) {
                console.error('There was an error initializing the publisher: ', initErr.name, initErr.message);
                return;
            }
        });

        // Connect to the session
        session.connect(token, function callback(error) {
            // If the connection is successful, initialize a publisher and publish to the session
            if (!error) {
                // If the connection is successful, publish the publisher to the session
                session.publish(publisher, function publishCallback(publishErr) {
                    if (publishErr) {
                        console.error('There was an error publishing: ', publishErr.name, publishErr.message);
                    }
                });
                if (session.connection && session.connection.id) {
                    elmApp.ports.receiveConnectionId.send(session.connection.id);
                }
            } else {
                console.error('There was an error connecting to the session: ', error.name, error.message);
            }
        });

        // Receiving a message (signal) ... 
        session.on('signal:msg', function signalCallback(event) {
            elmApp.ports.messageReceiver.send(event);
        });

        session.on('signal:conn', function signalCallback(event) {
            var splits = event.data.split('|');
            var payload =
            {
                "streamName": splits[0],
                "connectionId": splits[1]
            };
            elmApp.ports.newStreamConnected.send(payload);
        });

        session.on('signal:reaction', function signalCallback(event) {
            console.log("why are you so reactive?!?!?!");

            var splits = event.data.split('|');
            var payload =
            {
                "connectionId": splits[0],
                "chatMsgId": splits[1]
            };
            elmApp.ports.newReaction.send(payload);
        });

    });

    // tracking SDK being `null` indicates tracking will be ignored/unsent

    function signalNewStreamConnection(event) {
        if (session) {
            session.signal({
                type: 'conn',
                data: `${event.stream.name}|${event.stream.connection.id}`
            }, function signalCallback(error) {
                if (error) {
                    console.error('Error sending signal:', error.name, error.message);
                }
            });

        } else {
            console.log("Session is available");
        }
    }

    /**
     * port sendMessage : String -> Cmd msg
     */
    elmApp.ports.sendMessage.subscribe(function (txtMsg) {
        if (session) {
            var delimiter = "‡";
            var correlationId = nanoid();

            session.signal({
                type: 'msg',
                data: `${correlationId}${delimiter}${txtMsg}`
            }, function signalCallback(error) {
                if (error) {
                    console.error('Error sending signal:', error.name, error.message);
                }
            });
        } else {
            console.log("Session is available");
        }
    });

    /**
     * port reactToMessage : ( String, String ) -> Cmd msg
     */
    elmApp.ports.reactToMessage.subscribe(function (reactionTuple) {
        if (session) {
            var [connId, chatMsgId] = reactionTuple;

            session.signal({
                type: 'reaction',
                data: `${connId}|${chatMsgId}`
            }, function signalCallback(error) {
                if (error) {
                    console.error('Error sending signal:', error.name, error.message);
                }
            });
        } else {
            console.log("Session is available");
        }
    });
}