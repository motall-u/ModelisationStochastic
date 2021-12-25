
import umontreal.ssj.simevents.*;
import umontreal.ssj.rng.*;
import umontreal.ssj.stat.*;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;

import umontreal.ssj.randvar.*;

public class ReplayDay {
	
	//input fichier excel
	//A determiner:
	//longueur de la file
	// longueur des autres files
	// LES : temps attente du dernier client entree en service
	// 
	LinkedList<Customer> file[] = new LinkedList[8];
	double les[] = new double[8];
	//final_list contient les clients deja servis
	ArrayList<Customer> final_list = new ArrayList<Customer>();

	class Customer{
		int customer_type;
		double arrival_time;
		double begin_service_time;
		double end_service_time;
		int number_of_agent;
		double waiting_time;
		double hangup_time;
		double les;
		int queues_size[] = new int[8]; //stocke la longueur des fils
		
		public int getCustomer_type() {
			return customer_type;
		}
		public void setCustomer_type(int customer_type) {
			this.customer_type = customer_type;
		}
		public double getArrival_time() {
			return arrival_time;
		}
		public void setArrival_time(double arrival_time) {
			this.arrival_time = arrival_time;
		}
		public double getBegin_service_time() {
			return begin_service_time;
		}
		public void setBegin_service_time(double begin_service_time) {
			this.begin_service_time = begin_service_time;
		}
		public double getEnd_service_time() {
			return end_service_time;
		}
		public void setEnd_service_time(double end_service_time) {
			this.end_service_time = end_service_time;
		}
		public int getNumber_of_agent() {
			return number_of_agent;
		}
		public void setNumber_of_agent(int number_of_agent) {
			this.number_of_agent = number_of_agent;
		}
		public double getWaiting_time() {
			return waiting_time;
		}
		public void setWaiting_time(double waiting_time) {
			this.waiting_time = waiting_time;
		}
		public double getLes() {
			return les;
		}
		public void setLes(double les) {
			this.les = les;
		} 
		
		
		public double getHangup_time() {
			return hangup_time;
		}
		public void setHangup_time(double hangup_time) {
			this.hangup_time = hangup_time;
		}
		public double getTimeToLeaveQueue() {
			double val;
			if (begin_service_time==0) {
				val = hangup_time - arrival_time;
			}else {
				val = begin_service_time - arrival_time;
			}
			return val;
			
		}
		
	}
	
	public double DateToSecond(String d) {

		String date[]  = d.split(":");
//		System.out.println(date[0]);
		int h= Integer.parseInt(date[0]);
		int m = Integer.parseInt(date[1]);
		int s = Integer.parseInt(date[2]);
		return h*3600 + m*60 + s;
		
	}
	
	//INitialise la date avec la condition null ou pas pour answer, hangup

	
	public void ReadFileAndCreateCustomer(String file_jour_url) throws IOException {
		BufferedReader br = new BufferedReader(new FileReader(file_jour_url));
		
//		String ReadLine = br.readLine();
		String ReadLine = "";
		while ((ReadLine = br.readLine()) != null) {
	        
//		}
//		while(ReadLine!=null){
			ReadLine = br.readLine();
			String elements[] = ReadLine.split(",");
			String date_received = (elements[0]).split(" ")[1];
			
			String callType = elements[1];
			String agentNumber = elements[2];
			String answered = (elements[3]).split(" ")[1];
			
			String hangup = (elements[4]).split(" ")[1];
			int numberOfAgent =Integer.parseInt(elements[5]);
			Customer c = new Customer();
			c.setCustomer_type(Integer.parseInt(callType));
			c.setArrival_time(DateToSecond(date_received));
			c.setBegin_service_time(DateToSecond(answered));	
			c.setHangup_time(DateToSecond(hangup));
			c.setEnd_service_time(DateToSecond(hangup));
			c.setWaiting_time(c.getTimeToLeaveQueue());
			c.setNumber_of_agent(numberOfAgent);
//			System.out.println(numberOfAgent);
			// Prevoir l'evenement arrive du client dans la fils
			new Arrival(c).schedule(c.arrival_time);
		}
		
	}
	
	//Recupere la cle en fonction du type customer
	public int getKeyFromType(int type) {

		int val = 10;
		if (type == 30175){
			val = 0;
		}else if(type == 30172) {
			val = 1;
		}else if(type == 30560) {
			val = 2;
		}else if(type == 30181) {
			val =3;
		}else if(type ==30179) {
			val =4;
		}else if(type == 30066) {
			val = 5;
		}else if (type ==30241) {
			val=6;
		}else if(type == 30584) {
			val =7;
		}
		
		return val;
		
	}
	
	//Arrive du client au niveau de la file
	class Arrival extends Event{
		Customer cust;
	
		public Arrival (Customer c) {
			cust = c;
//			System.out.println(cust.arrival_time);
		}
		
		
		@Override
		public void actions() {
			
			//Placer le client au niveau de sa file d'attente
			for (int i = 0; i < file.length; i++) {
				cust.queues_size[i] = file[i].size();
			}
			//Recuperer l'index du customer a partir du type
			int type = getKeyFromType(cust.getCustomer_type());
//			System.out.println(type);
			(file[type]).add(cust);
			//Set LES
			cust.setLes(les[type]);
			
//			System.out.println(cust.getTimeToLeaveQueue());
			new Depart(cust).schedule(cust.getTimeToLeaveQueue());
			
		}
		
	}
	
	//Depart de la file
	class Depart extends Event{
		Customer cust = null ;
		public Depart(Customer c) {
			cust = c;
		}

		@Override
		public void actions() {
			int type = getKeyFromType(cust.getCustomer_type());
			file[type].remove(cust);
			if (cust.getBegin_service_time() != -1) {
				les[type] = cust.getWaiting_time();
				new EndOfService(cust).schedule(cust.end_service_time - cust.begin_service_time);
			}
			les[type] = cust.getTimeToLeaveQueue();
				
		}
		
	}
	
	class EndOfService extends Event{
		Customer cust=null;
		public EndOfService(Customer c) {
			// TODO Auto-generated constructor stub
			cust =c;
		}
		@Override
		public void actions() {
			final_list.add(cust);
			
			
		}
		
	}
	
	public void InitializeQueue() {
		for (int i = 0; i < file.length; i++) {
			file[i] = new LinkedList<Customer>();
		}
	}
	
	
	public String QueuesSizeToString(Customer c) {
		String info = "";
		for (int i = 0; i < c.queues_size.length; i++) {
			info = info + c.queues_size[i] + ",";
		}
		return info;
	}
	public void FinalListToCsv(String filename) throws IOException {
		BufferedWriter bw = new BufferedWriter(new FileWriter(filename));
		
		for( Customer c: final_list) {
			String info_cust = c.getCustomer_type() + ","
					+ c.getArrival_time() + ","
					+ QueuesSizeToString(c)
					+ c.getLes() + ","
					+ c.getNumber_of_agent() + ","
					+ c.getWaiting_time();
			System.out.println(info_cust);
			bw.write(info_cust);
			bw.newLine();
		}
		bw.close();
		
	}
	public static void main(String[] args) throws IOException {
		// lOOP FOR THE TWELVE MONTHS 01 TO 12
			for (int j = 1; j < 29; j++) {
				
				try {
					Sim.init();
					
					//Initialize queue
					ReplayDay simulation = new ReplayDay();
					simulation.InitializeQueue();
					String path_to_file = "/home/motall/Bureau/DIC2/M.Stochastique/nouveau/calls-2014-12/"+j+".csv";
					simulation.ReadFileAndCreateCustomer(path_to_file);
//					System.out.println(path_to_file);
					
					Sim.start();
					
					simulation.FinalListToCsv("calls-2014-01-"+j+".csv");
				
				} catch (Exception e) {
					// TODO: handle exception
				}
				
			}
	
					


	}

}
