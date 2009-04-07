
Are you running Yaws as your web server and want to be able to
run different Yaws backend nodes, either on the same Host or on
different hosts?

Then *mbrain* may be the easy solution for you. 

For example, on my server at home I'm running a number of experimental 
systems written in Erlang. Many of them have a web interface that I 
want to expose to the Internet. And I want them to be accessible as 
subdomains of MY.OWN.DOMAIN, on port 80.

The idea is very simple. On my server I will run only one Erlang node
that runs Yaws. This is my web server. I configure Yaws to virt-host 
my systems, pointing the docroot's accordingly. Then for each backend 
system I make sure that every *.yaws* file only contains one single 
rpc-call to the actual backend system node. The result from the rpc-call 
must be consumable by the Yaws server. Typically, this means that I
move any Yaws code into a corresponding Erlang module. So for example,
if I have a *login.yaws* file, then I move the content of the file into
an Erlang module named: *login_yaws.erl*. The only thing left in the 
login.yaws file is the following code:

    out(Arg) -> mbrain:call(mynode, login_yaws, out, [Arg]).

The *mbrain:call/4* function, which executes in the mbrain frontend
server node, will check if there exist any other connected nodes that 
match *mynode@HOST*, and if so; make an rpc:call/4 to that node. 
If there are several node names that match, one of them is picked 
by random. It is also possible to specify a list of node names to 
the *mbrain:call/4* to cater for the case where you want to load 
balance several nodes on the same host. Example: 

    out(Arg) -> mbrain:call([mynode1,mynode2], login_yaws, out, [Arg]).

This means that none of my systems anylonger need to run Yaws of 
their own. It also results in that no traditional rev-proxy work, 
a la nginx, is needed. And as the final touch; it also makes it 
possible to implement a simple form of load balancing. 

The only thing that the backend node(s) need to do is to 
call the *mbrain:ping()* function (or *mbrain:ping(Host)* ).
This will setup a connection to the *mbrain* node and a
watcher process will be started that ensures that this
connection will stay up. 

Isn't Erlang wonderful :-)

--Tobbe
