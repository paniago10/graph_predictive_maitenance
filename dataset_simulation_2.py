import numpy as np
import random
from datetime import date
from scipy.interpolate import interp1d
import pandas as pd
#os valores das faixas de parâmetros são para um ventilador axial
#com acionamento de motor de 10 Kw
#Os parâmetros máximos são:
#Vibração: 0,5 mm/s (RMS)
#Temperatura: 120 ºC
#Amperagem: 30 A
#Velocidade do ar (mínima): 2m/s


class MachineDataSample:
    CUTOFF = 150

    def __init__(self, rotation_rpm, vibration_rms, temperature, air_speed, amperage, time_used, sample_rate=1024,failure=0):
        self.sample_rate = sample_rate
        self.rotation_rpm = rotation_rpm
        self.vibration_rms = vibration_rms
        self.temperature = temperature
        self.air_speed = air_speed
        self.amperage = amperage
        self.time_used = time_used
        self.__N = int(time_used) * 60 * sample_rate
        self.failure = failure  # Initialize failure as 0 (no failure)

    def pcm(self):
        ts = np.linspace(0, self.time_used * 60, num=self.__N, endpoint=False)

        # Interpolate parameters
        rpm = interp1d([0, self.time_used * 60], [self.rotation_rpm, self.rotation_rpm])
        vibration = interp1d([0, self.time_used * 60], [self.vibration_rms, self.vibration_rms])
        temperature = interp1d([0, self.time_used * 60], [self.temperature, self.temperature])
        air_speed = interp1d([0, self.time_used * 60], [self.air_speed, self.air_speed])
        amperage = interp1d([0, self.time_used * 60], [self.amperage, self.amperage])

        

        a = np.sin(2 * np.pi * rpm(ts)) * vibration(ts)
        a[a > self.CUTOFF] = self.CUTOFF
        a[a < -self.CUTOFF] = -self.CUTOFF

        return np.int16(a / self.CUTOFF * 32767)

# Example of generating machine data samples
# Create a list to store machine data samples
machine_data_samples = []

# Define the parameters for each machine data sample
n_samples = 6000  # Number of data samples
for _ in range(n_samples):
    rotation_rpm = random.uniform(1200, 1800)
    vibration_rms = random.normalvariate(0.3, 0.1)
    temperature = random.normalvariate(98, 10)
    air_speed = random.normalvariate(2.0, 0.5)
    amperage = random.normalvariate(25, 2.5)
    time_used = np.round(random.normalvariate(1000, 100))  # Time of use in hours
    failure=0
    # Simulate failure based on time_used e condições
    alpha=1.0
    beta=500
    if  time_used >= np.round(1200+beta * np.random.weibull(alpha)):
        failure = 1  # Set failure to 1 if conditions are met
    if temperature > 120:
        failure = 1  # Set failure to 1 if conditions are met
    if air_speed < 0.6:
        failure = 1  # Set failure to 1 if conditions are met
    if vibration_rms > 0.5:
        failure = 1  # Set failure to 1 if conditions are met
    if amperage >30:
        failure = 1  # Set failure to 1 if conditions are met
    # Create a machine data sample
    
    machine_sample = MachineDataSample(rotation_rpm, vibration_rms, temperature, air_speed, amperage, time_used, sample_rate=1024, failure=failure)


    # Append the machine data sample to the list
    machine_data_samples.append(machine_sample)

# Create a DataFrame to store machine data samples

data = pd.DataFrame({
    'Rotation_RPM': [sample.rotation_rpm for sample in machine_data_samples],
    'Vibration_RMS': [sample.vibration_rms for sample in machine_data_samples],
    'Temperature_C': [sample.temperature for sample in machine_data_samples],
    'Air_Speed_m_s': [sample.air_speed for sample in machine_data_samples],
    'Amperage_A': [sample.amperage for sample in machine_data_samples],
    'Time_Used_h': [sample.time_used for sample in machine_data_samples],
    'Failure': [sample.failure for sample in machine_data_samples]
})

# Save the DataFrame to a CSV file
data.to_csv('D:\\DISCO C\\Argélio\\Preparação de Aulas\\IFSP\\Iniciação Científica\\2023\\sistemas Complexos\\R_Deep_predictive\\machine_data_samples_1.csv', index=False)
data.to_excel('D:\\DISCO C\\Argélio\\Preparação de Aulas\\IFSP\\Iniciação Científica\\2023\\sistemas Complexos\\R_Deep_predictive\\machine_data_samples_1.xlsx', index=False)
