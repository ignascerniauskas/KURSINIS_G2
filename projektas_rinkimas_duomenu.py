# from bs4 import BeautifulSoup
# import pandas as pd

# # File paths for the two different HTML files
# file_path1 = '2019_2020_pavadinimai.txt'
# file_path2 = '2019_2020_mokiniai_kiekis.txt'
# file_path3 = '2020_2021_pavadinimai.txt'
# file_path4 = '2020_2021_mokiniai_kiekis.txt'
# file_path5 = '2021_2022_pavadinimai.txt'
# file_path6 = '2021_2022_mokiniai_kiekis.txt'
# file_path9 = '2023_2024_pavadinimai.txt'
# file_path10 = '2023_2024_mokiniai_kiekis.txt'
# file_path7 = '2022_2023_pavadinimai.txt'
# file_path8 = '2022_2023_mokiniai_kiekis.txt'

# def extract_data(file_path):
#     # Read the HTML content from the file
#     with open(file_path, 'r', encoding='utf-8') as file:
#         html_content = file.read()

#     # Parse the HTML using BeautifulSoup
#     soup = BeautifulSoup(html_content, 'html.parser')

#     # Extract data assuming they are stored within <span> tags with a specific attribute
#     #data = [span.get_text() for span in soup.find_all('span', {'specname': 'textItem'}) if span.get_text() != 'Iš viso']
#     data = [span.get_text() for span in soup.find_all('span', {'specname': 'textItem'}) ]
#     return data

# # Extract data from both files
# data1 = extract_data(file_path1)
# data2 = extract_data(file_path2)

# # Create a DataFrame
# # The longest list determines the number of columns. Shorter lists are padded with NaN.
# max_len = max(len(data1), len(data2))
# columns = {'File1': data1 + [None] * (max_len - len(data1)), 'File2': data2 + [None] * (max_len - len(data2))}
# df = pd.DataFrame(columns)

# # Save to Excel
# excel_path = 'extracted_data.xlsx'
# df.to_excel(excel_path, index=False)

# print(f"Data has been saved to {excel_path}")


# from bs4 import BeautifulSoup
# import pandas as pd

# file_paths = [
#     ('2019_2020_pavadinimai.txt', '2019_2020_mokiniai_kiekis.txt'),
#     ('2020_2021_pavadinimai.txt', '2020_2021_mokiniai_kiekis.txt'),
#     ('2021_2022_pavadinimai.txt', '2021_2022_mokiniai_kiekis.txt'),
#     ('2022_2023_pavadinimai.txt', '2022_2023_mokiniai_kiekis.txt'),
#     ('2023_2024_pavadinimai.txt', '2023_2024_mokiniai_kiekis.txt')
# ]

# def extract_data(file_path):
#     # Read the HTML content from the file
#     with open(file_path, 'r', encoding='utf-8') as file:
#         html_content = file.read()

#     # Parse the HTML using BeautifulSoup
#     soup = BeautifulSoup(html_content, 'html.parser')

#     # Extract data assuming they are stored within <span> tags with a specific attribute
#     data = [span.get_text() for span in soup.find_all('span', {'specname': 'textItem'})]
#     return data

# # Prepare to write to Excel
# excel_path = 'mokinių_kiekis.xlsx'
# writer = pd.ExcelWriter(excel_path, engine='xlsxwriter')

# # Process each pair of files and write to separate sheets
# for i, (path1, path2) in enumerate(file_paths, start=1):
#     data1 = extract_data(path1)
#     data2 = extract_data(path2)
#     max_len = max(len(data1), len(data2))
#     columns = {
#         'Pavadinimas': data1 + [None] * (max_len - len(data1)), 
#         'Kiekis': data2 + [None] * (max_len - len(data2))
#     }
#     df = pd.DataFrame(columns)
#     df.to_excel(writer, sheet_name=f'School Data {i}', index=False)

# # Save and close the Excel writer
# writer.close()

# print(f"Išsaugota į skirtignus puslapius {excel_path} ")










































#mokinia~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# from bs4 import BeautifulSoup
# import pandas as pd

# # Apibrėžiami failų keliai poromis, kiekvienai mokslo metų porai atskirai
# file_paths = [
#     ('2019_2020_pavadinimai.txt', '2019_2020_mokiniai_kiekis.txt'),
#     ('2020_2021_pavadinimai.txt', '2020_2021_mokiniai_kiekis.txt'),
#     ('2021_2022_pavadinimai.txt', '2021_2022_mokiniai_kiekis.txt'),
#     ('2022_2023_pavadinimai.txt', '2022_2023_mokiniai_kiekis.txt'),
#     ('2023_2024_pavadinimai.txt', '2023_2024_mokiniai_kiekis.txt')
# ]

# def extract_data(file_path):
#     # Nuskaitomas HTML turinys iš failo
#     with open(file_path, 'r', encoding='utf-8') as file:
#         html_content = file.read()

#     # HTML turinys analizuojamas naudojant BeautifulSoup
#     soup = BeautifulSoup(html_content, 'html.parser')

#     # Ištraukiami duomenys iš <span> žymių su tam tikru atributu
#     data = [span.get_text() for span in soup.find_all('span', {'specname': 'textItem'})]
#     return data

# # Paruošiama rašyti duomenis į Excel
# excel_path = 'mokinių_kiekis.xlsx'
# writer = pd.ExcelWriter(excel_path, engine='xlsxwriter')

# # Apdorojama kiekviena failų pora ir duomenys įrašomi į atskirus Excel lapus
# for i, (path1, path2) in enumerate(file_paths, start=1):
#     data1 = extract_data(path1)
#     data2 = extract_data(path2)
#     max_len = max(len(data1), len(data2))
#     columns = {
#         'Pavadinimas': data1 + [None] * (max_len - len(data1)), 
#         'Kiekis': data2 + [None] * (max_len - len(data2))
#     }
#     df = pd.DataFrame(columns)
#     df.to_excel(writer, sheet_name=f'Mokyklų duomenys {i}', index=False)

# # Excel failas uždaromas ir išsaugomas
# writer.close()

# # Atspausdinama žinutė apie sėkmingą duomenų įrašymą
# print(f"Išsaugota į skirtignus puslapius {excel_path}")












from bs4 import BeautifulSoup
import pandas as pd

# Apibrėžiami failų keliai poromis, kiekvienai mokslo metų porai atskirai
file_paths = [
    ('finansai_2019_2020.txt', 'finansai_gimnazijos.txt'),
    ('finansai_2020_2021.txt', 'finansai_gimnazijos.txt'),
    ('finansai_2021_2022.txt', 'finansai_gimnazijos.txt'),
    ('finansai_2022_2023.txt', 'finansai_gimnazijos.txt'),
    ('finansai_2023_2024.txt', 'finansai_gimnazijos.txt')
]

def extract_data(file_path):
    # Nuskaitomas HTML turinys iš failo
    with open(file_path, 'r', encoding='utf-8') as file:
        html_content = file.read()

    # HTML turinys analizuojamas naudojant BeautifulSoup
    soup = BeautifulSoup(html_content, 'html.parser')

    # Ištraukiami duomenys iš <span> žymių su tam tikru atributu
    data = [span.get_text() for span in soup.find_all('span', {'specname': 'textItem'})]
    return data

# Paruošiama rašyti duomenis į Excel
excel_path = 'finansavimas.xlsx'
writer = pd.ExcelWriter(excel_path, engine='xlsxwriter')

# Apdorojama kiekviena failų pora ir duomenys įrašomi į atskirus Excel lapus
for i, (path1, path2) in enumerate(file_paths, start=1):
    data1 = extract_data(path1)
    data2 = extract_data(path2)
    max_len = max(len(data1), len(data2))
    columns = {
        'Pavadinimas': data1 + [None] * (max_len - len(data1)), 
        'Kiekis': data2 + [None] * (max_len - len(data2))
    }
    df = pd.DataFrame(columns)
    df.to_excel(writer, sheet_name=f'Mokyklų duomenys {i}', index=False)

# Excel failas uždaromas ir išsaugomas
writer.close()

# Atspausdinama žinutė apie sėkmingą duomenų įrašymą
print(f"Išsaugota į skirtignus puslapius {excel_path}")
