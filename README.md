# cartographer-server

An executable program that maps out a network of Eureka-based, cooperating
microservice by querying Eureka for special metadata, and implementing an HTTP
service that renders a Graphviz diagram showing the interactions between the
services.

Its purpose is to provide a real-time view of what microservices exist in
a deployment environment, and how they interact. The current version is
limited in the type of information it can display, but we have a vision
of extending this in the future.

Cartographer requires some level of participation from the individual
microservices that make up the network. Specifically, microservices must
register cartographer data with Eureka.

## Participant specification.

Each participating service instance must register a special Eureka metadata value
containing the cartographer data for that service.

The well-known Eureka metadata key is the value "cartography"

The format of the cartographer data is an embedded graphviz DOT document
describing the relationships between the participating service and other
services with which it communicates.

Example:

    digraph {
        "my-service" -> "some-supporting-service";
        "my-service" -> "eureka";
    }

By convention, the name of the nodes should match the name of the application
name with which the instance registered itself with Eureka.

Sometimes, it is desirable to describe interaction with nodes that exist
outside of your local environment, such as third-party apis or services. Some
people like to specify these kinds of external services using fancy node
styles.

Example:

    digraph {
        "third-party-api" [style=dashed];
        "my-service" -> "third-party-api";
        "my-service" -> "some-supporting-service";
        "my-service" -> "eureka";
    }


## Current limitations

Currently, Cartographer doesn't do much more than round up and merge all
of the graph segments, so its correctness is in large part dependent on
the participants providing good and correct information.

## Future Plans

Ideas for future features mainly revolve around adding augmented metadata,
including:

    - The number of instances of any particular service.
    - Sub-graphs which break down the various versions of any particular
      service.

