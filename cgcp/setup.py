from setuptools import setup, find_packages


with open('README.md') as f:
    readme = f.read()

setup(
    name='cgcp',
    version='0.1.0',
    description='All code for CorpGov and CompPerf',
    long_description=readme,
    author='Conor Reid',
    author_email='conor.reid@ucdconnect.ie',
    url='https://github.com/ReidConor/dissertation',
    packages=find_packages(exclude=('docs'))
)
